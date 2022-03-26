;; Copyright (C) 2018  Matthias Paulmier

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

;; Set lexical-binding to true by default
(setq-default lexical-binding t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)

(require 'init-base)
(require 'init-ui)

(require 'init-calendar)
(require 'init-comint)
(require 'init-company)
(require 'init-completion)
(require 'init-dired)
(require 'init-edit)
(require 'init-embark)
(require 'init-flycheck)
(require 'init-ibuffer)
(require 'init-lisp)
(require 'init-lsp)
(require 'init-magit)
(require 'init-markup)
(require 'init-mc)
(require 'init-prog)
(require 'init-python)
(require 'init-rust)
(require 'init-which-key)
(require 'init-yasnippet)
