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

(use-package iedit
  :diminish iedit-mode
  :bind (("C-;" . iedit-mode)
	 ("C-M-;" . iedit-dwim))
  :init
  ;; taken from the best : https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
	(iedit-mode)
      (save-excursion
	(save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))

(use-package csv-mode
  :init
  (setq-default csv-separators '(";" "\t")))

(setq-default show-trailing-whitespace t
              truncate-lines t
              major-mode 'text-mode
              fill-column 80
              display-fill-column-indicator t
              display-fill-column-indicator-character "│"
              tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'complete
              sort-fold-case t)

(global-display-fill-column-indicator-mode +1)
(global-set-key (kbd "C-c M-l") #'sort-lines)

(provide 'init-edit)
