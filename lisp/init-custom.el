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

(defgroup mp nil
  "Mattplm's personal config's customization"
  :group 'convenience)

(defcustom mp/org-directory (concat (getenv "HOME") "/Org/")
  "Main org directory"
  :group 'mp
  :type 'string)

(defcustom mp/no-stw-modes '(calendar-mode comint-mode help-mode markdown-mode)
  "List of modes for which we wish to disable
`show-trailing-whitespace'"
  :group 'mp
  :type '(repeat symbol))

(defcustom mp/no-dtw-modes '(markdown-mode)
  "List of modes for which we wish to save files without running
`delete-trailing-whitespace'"
  :group 'mp
  :type '(repeat symbol))

(provide 'init-custom)
