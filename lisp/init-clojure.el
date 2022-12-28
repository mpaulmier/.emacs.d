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


(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook
  (clojure-mode . enable-paredit-mode)
  (clojure-mode . subword-mode))

(use-package cider
  :after clojure-mode
  :hook
  (cider-mode . eldoc-mode)
  (cider-repl-mode . paredit-mode)
  :bind (:map clojure-mode-map
              ("C-x C-e" . cider-eval-last-sexp)
              ("C-c C-k" . cider-load-buffer)
              ("C-c C-c" . cider-eval-defun-at-point))
  :custom
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-wrap-history t))

(provide 'init-clojure)
