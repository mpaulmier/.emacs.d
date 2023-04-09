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

(use-package move-text
  :bind (:map prog-mode-map
         ("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package subword
  :diminish subword-mode
  :init
  (global-subword-mode 1))

(setq-default show-trailing-whitespace t
              truncate-lines t
              major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

(provide 'init-edit)
