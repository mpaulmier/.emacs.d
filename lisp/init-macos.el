;; Copyright (C) 2024  Matthias Paulmier

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

(use-package ns-win
  :ensure nil
  :custom
  (mac-right-option-modifier "alt")
  :init
  (global-set-key [kp-delete] 'delete-char))

(use-package magit
  :init
  ;; Small hack for macos, don't do this maybe
  (defadvice server-ensure-safe-dir (around
                                     my-around-server-ensure-safe-dir
                                     activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it)))

(provide 'init-macos)
