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

(use-package python
  :ensure nil
  :hook
  (python-mode . flycheck-mode)
  :custom
  (python-check-command "flake8 --max-complexity 15")
  :bind (:map python-mode-map
         ("M-<right>" . python-indent-shift-right)
         ("M-<left>" . python-indent-shift-left)
         ("M-r" . python-debug-exception-dwim))
  :config
  (setq flycheck-flake8-maximum-complexity 15)
  (setq flycheck-flake8rc ".flake8")

  ;; TODO: implement this
  (defun python-debug-exception-dwim (&args rest)
    "Surround region or line with a try-except block with rpdb in the
except block to debug a suspect piece of code"
    (interactive)
    nil)
  (python-skeleton-define method nil
    "Function name: "
    "@classmethod" \n
    "def " str "(cls" ("Parameter, %s: "
                       str) "):" \n
                       "'''" - "'''" \n
                       > _ \n))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp-deferred)))
;;   :custom (lsp-pyright-auto-import-completions nil))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t)

(provide 'init-python)
