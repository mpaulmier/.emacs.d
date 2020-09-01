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

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)))

(use-package consult
  :bind (("M-y" . consult-yank-from-kill-ring)
         ("M-g M-g" . consult-goto-line)
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-S-s" . consult-ripgrep)
         ("C-S-x C-S-f" . consult-find)))

(use-package save-hist
  :ensure nil
  :init
  (savehist-mode 1))

(provide 'init-completion)
