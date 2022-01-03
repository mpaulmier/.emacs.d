;; Copyright (C) 2021  Matthias Paulmier

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

(require 'init-const)
(require 'init-functions)

(setq inhibit-startup-message t)
(setq auto-save-default nil
      backup-directory-alist `((".*" . ,mp/emacs-tmp-dir))
      create-lockfiles nil
      custom-file mp/custom-file)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

(setq cursor-in-non-selected-windows t
      delete-by-moving-to-trash t
      confirm-kill-emacs 'yes-or-no-p
      indicate-empty-lines t
      left-margin-width 1
      right-margin-width 1
      mouse-yank-at-point t
      help-window-select t
      scroll-conservatively most-positive-fixnum
      select-enable-clipboard t
      window-combination-resize t
      ring-bell-function nil
      visible-bell nil
      vc-follow-symlinks t)

(setq-default show-trailing-whitespace t)

(global-subword-mode 1)
(blink-cursor-mode -1)
(delete-selection-mode t)
(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Delete trailing whitespaces before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'mp/save-executable-buffer)

;; Disable show-trailing-whitespace for minibuffer
(add-hook 'minibuffer-setup-hook #'mp/disable-show-trailing-whitespace)

(when (file-exists-p mp/custom-file)
  (load-file mp/custom-file))
(put 'narrow-to-region 'disabled nil)

;; Global key bindings that cannot be attributed to specific packages
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-set-key (kbd "C-x C-r") #'mp/rename-current-buffer-file)
(global-set-key (kbd "C-c e") #'mp/browse-emacs-conf-dir)
(global-set-key (kbd "C-S-x C-S-e") #'mp/eval-and-replace)

(use-package lice)

(provide 'init-base)
