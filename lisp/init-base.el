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

(require 'init-functions)

(setq inhibit-startup-message t)
(setq auto-save-default nil
      backup-directory-alist `((".*" . ,mp/emacs-tmp-dir))
      create-lockfiles nil
      custom-file mp/custom-file)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(setq delete-by-moving-to-trash t
      confirm-kill-emacs 'yes-or-no-p
      mouse-yank-at-point t
      help-window-select t
      scroll-conservatively most-positive-fixnum
      select-enable-clipboard t
      window-combination-resize t
      ring-bell-function nil
      visible-bell nil)

(setq-default show-trailing-whitespace t
              cursor-in-non-selected-windows t
              indicate-empty-lines t
              left-margin-width 1
              right-margin-width 1
              truncate-lines t
              abbrev-mode t
              vc-follow-symlinks t)

(diminish 'abbrev-mode)

(add-hook 'text-mode-hook #'mp/disable-stw-maybe)

(blink-cursor-mode -1)
(delete-selection-mode t)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook #'mp/delete-trailing-whitespace-maybe)
(add-hook 'before-save-hook #'mp/save-executable-buffer)

;; Disable show-trailing-whitespace for minibuffer
(add-hook 'minibuffer-setup-hook #'mp/disable-stw-maybe)

(when (file-exists-p mp/custom-file)
  (load-file mp/custom-file))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Global key bindings that cannot be attributed to specific packages
(global-set-key (kbd "<f5>") #'revert-buffer-quick)
(global-set-key (kbd "C-x C-r") #'mp/rename-current-buffer-file)
(global-set-key (kbd "C-c e") #'mp/browse-emacs-conf-dir)
(global-set-key (kbd "C-S-x C-S-e") #'mp/eval-and-replace)
(global-set-key (kbd "C-=") #'mp/increment-number-at-point)
(global-set-key (kbd "C--") #'mp/decrement-number-at-point)
(global-set-key (kbd "M-S-<SPC>") #'mp/join-line-one-space)
(global-set-key (kbd "C-c s") #'mp/suggify-region)
(global-set-key (kbd "C-a") #'mp/beginning-of-line-or-indent)
(global-set-key (kbd "C-S-a") #'move-beginning-of-line)

;; Scroll horizontally
(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
				                    (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
				    (if truncate-lines (scroll-left 1))))
(global-set-key (kbd "C-x w") #'count-words)

(use-package lice)

(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t))

(provide 'init-base)
