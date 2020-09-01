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

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c o" . mp/dired-xdg-open-file))
  :custom (dired-listing-switches "-FlaGhv")
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (defun mp/dired-xdg-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file))))

(provide 'init-dired)
