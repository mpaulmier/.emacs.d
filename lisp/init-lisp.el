;; Copyright (C) 2020  Matthias Paulmier

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

(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :init
  (setq show-paren-style 'paren))

(use-package slime
  :if (executable-find "sbcl")
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package lisp-mode
  :ensure nil
  :diminish lisp-mode
  :mode "\\.cl\\|\\.lisp\\'")

(provide 'init-lisp)
