;; Copyright (C) 2022  Matthias Paulmier

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Mini buffer completions

(use-package files
  :ensure nil
  :bind ("C-x C-i" . mp/insert-file-name))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  :config
  ;; Ring bell on cycle
  (advice-add #'vertico-next
              :around
              #'(lambda (origin &rest args)
                  (let ((beg-index vertico--index))
                    (apply origin args)
                    (if (not (eq 1 (abs (- beg-index vertico--index))))
                        (let ((visible-bell t))
                          (ding)))))))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp)))

(use-package flymake
  :ensure nil
  :after consult
  :bind (:map flymake-mode-map
         ("C-c f" . consult-flymake)))

(use-package save-hist
  :ensure nil
  :init
  (savehist-mode 1))

;;; In buffer completions

(use-package eglot
  :commands eglot
  :bind (:map prog-mode-map
         ("s-l e" . eglot)
         :map eglot-mode-map
         ("s-l r" . eglot-rename)
         ("s-l a" . eglot-code-actions)
         ("s-l d" . flymake-show-buffer-diagnostics))
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pylsp")))))
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-extend-to-xref t)
  (setq eglot-events-buffer-size 0))

(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("M-RET" . tempel-really-end)
         ("TAB" . tempel-next))
  :init

  (defun tempel-really-end ()
    (interactive)
    (tempel-end)
    (tempel-done))

  (defun tempel-setup-capf ()
    (when (not (member #'tempel-complete completion-at-point-functions))
        (setq-local completion-at-point-functions
                    (cons #'tempel-complete
                          completion-at-point-functions))))
  (add-hook 'nxml-mode-hook #'tempel-setup-capf))

(use-package cape
  :after (eglot tempel)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; From corfu's wiki
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun mp/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'mp/eglot-capf))

(use-package corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay 0.1)
  :init
  (global-corfu-mode))

(provide 'init-completion)
