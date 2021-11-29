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

(use-package python
  :ensure nil
  :hook
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  (python-mode . yas-minor-mode)
  :bind (:map python-mode-map
         ("M-<right>" . python-indent-shift-right)
         ("M-<left>" . python-indent-shift-left)))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(provide 'init-python)
