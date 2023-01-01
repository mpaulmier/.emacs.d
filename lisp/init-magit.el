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

(use-package magit
  :if (executable-find "git")
  :bind (("C-c g" . magit-status)
	     ("C-c G" . magit-blame-addition)
	     ("C-c i" . magit-init)

	     :map magit-status-mode-map
	     ("q" . mp/magit-quit-session))
  :custom
  ;; Maximum acceptable width for summary buffer
  (git-commit-summary-max-length 50)
  (magit-bind-magit-project-status t)
  :init
  (require 'magit-extras)
  :config
  ;; full screen magit-status
  ;; From http://whattheemacsd.com/setup-magit.el-01.html
  (defun magit-status--ad-around (old-func &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply old-func args)
    (delete-other-windows))

  (dolist (fun '(magit-status magit-project-status))
    (advice-add fun :around #'magit-status--ad-around))

  (defun mp/magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package blamer
  :custom
  (blamer-self-author-name . ("You"))
  (blamer-force-truncate-long-line . nil)
  (blamer-type 'visual)
  :init
  (global-blamer-mode 1))

(provide 'init-magit)
