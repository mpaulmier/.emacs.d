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

(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

(use-package hideshow
  :diminish hs-minor-mode
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  (prog-mode . highlight-indent-guides-mode))

(use-package flymake
  :ensure nil
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dockerfile-mode)

(provide 'init-prog)
