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

(use-package consult
  :bind (("M-y" . consult-yank-from-kill-ring)
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-S-s" . consult-ripgrep)
         ("C-S-x C-S-f" . project-find-file)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu))
  :custom
  (consult-preview-key 'any)
  :config
  (setq consult-project-root-function
        #'(lambda ()
            (when-let (project (project-current))
              (car (project-roots project))))))

(use-package flymake
  :ensure nil
  :after consult
  :bind (:map flymake-mode-map
         ("C-c f" . consult-flymake)))

(use-package save-hist
  :ensure nil
  :init
  (savehist-mode 1))

(provide 'init-completion)
