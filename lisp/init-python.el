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
  (python-mode . flymake-start)
  (python-mode . python-ts-mode)
  (python-mode . eglot-ensure)
  :bind (:map python-mode-map
              ("M-<right>" . python-indent-shift-right)
              ("M-<left>" . python-indent-shift-left)
              ("C-c C-t d" . python-skeleton-method))
  :config
  (python-skeleton-define method nil
    "Function name: "
    "@classmethod" \n
    "def " str "(cls" ("Parameter, %s: "
                       str) "):" \n
    "'''" - "'''" \n
    > _ \n))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(provide 'init-python)
