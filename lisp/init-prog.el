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
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top))

(use-package prog
  :ensure nil
  :hook
  (python-mode . highlight-indent-guides-mode))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-flake8-maximum-complexity 15)
  :init
  ;; Run flycheck on the entire buffer when opening a file
  ;; This allows seeing the errors on the file without having to modify it first
  (add-hook 'flycheck-mode-hook
            #'(lambda ()
                (flycheck-buffer))))

(provide 'init-prog)
