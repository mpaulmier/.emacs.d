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

(use-package flycheck
  :diminish flycheck-mode
  :custom
  (flycheck-check-syntax-automatically '(save new-line))
  (flycheck-idle-change-delay 5.0)
  (flycheck-display-errors-delay 0.9)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-standard-error-navigation t)
  (flycheck-deferred-syntax-check nil)
  (flycheck-disabled-checkers '(python-pylint))
  (flycheck-python-flake8-executable "~/.pyenv/shims/flake8"))

(provide 'init-flycheck)
