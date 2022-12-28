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

(defun compute-ssn (ssn)
  "Takes the first 13 characters of a french social security number
and returns the full 15 chars social security number with its key"

  (interactive "sType the first 13 numbers of the ssn number here: ")
  (if (not (eq 13 (length ssn)))
      (error "SSN needs to be 13 chars long"))
  (let* ((ssn-number (string-to-number ssn))
         (key (- 97 (mod ssn-number 97))))
    (if (eq 0 ssn-number)
        (error "SSN must represent a number"))
    (concat ssn (string-pad (number-to-string key) 2 ?0 t))))

(defun do-edit-coog-changelogs ()
  (interactive)
  (if (not (s-contains? "/modules/" (buffer-file-name)))
      (error "Not in a coog module"))
  (global-set-key (kbd "C-c w c") 'do-end-edit-coog-changelogs)
  (let* ((root (car (project-roots (project-current))))
         (module (concat
                  root
                  (replace-regexp-in-string
                   ".*/\\(modules/[^/]*\\)/.*"
                   "\\1"
                   (buffer-file-name)))))
    (window-configuration-to-register :coog-changelog-edit)
    (delete-other-windows)
    (split-window-right)
    (find-file (concat module "/doc/en/CHANGELOG"))
    (rename-buffer "EDIT-EN-CHANGELOG-COOG")
    (beginning-of-buffer)
    (without-major-mode (open-line 1))
    (other-window 1)
    (find-file (concat module "/doc/fr/CHANGELOG"))
    (rename-buffer "EDIT-FR-CHANGELOG-COOG")
    (beginning-of-buffer)
    (without-major-mode (open-line 1))))

(defun do-end-edit-coog-changelogs ()
  (interactive)
  (switch-to-buffer "EDIT-EN-CHANGELOG-COOG")
  (save-buffer)
  (kill-this-buffer)
  (switch-to-buffer "EDIT-FR-CHANGELOG-COOG")
  (save-buffer)
  (kill-this-buffer)
  (jump-to-register :coog-changelog-edit)
  (global-set-key (kbd "C-c w c") 'do-edit-coog-changelogs))

(defun insert-coog-header ()
  (interactive)
  (let ((lice:header-spec '(lice:insert-license)))
    (lice "coog-header")))

(global-set-key (kbd "C-c w c") 'do-edit-coog-changelogs)
(global-set-key (kbd "C-c w l") 'insert-coog-header)

(use-package conf-mode
  :ensure nil
  :mode "\\.\\(po\\|conf\\)\\'")

(provide 'init-work)
