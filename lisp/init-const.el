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

(defconst mp/emacs-tmp-dir
  (format "%s%s%s/" temporary-file-directory "emacs" (user-uid))
  "Temporary files go here")

(defconst mp/custom-file
  (concat user-emacs-directory "custom.el")
  "Recipient for modifications made through `M-x customize'")

(provide 'init-const)
