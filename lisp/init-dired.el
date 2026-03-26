;;; init-dired.el --- Dired extensions -*- lexical-binding: t; -*-

;;; Code:

(defun mp/get-date nil
  (interactive)
  (if (eq system-type 'darwin)
      (shell-command-to-string "echo -n $(date -I)")
      (shell-command-to-string "echo -n $(date --iso)")))

(defun mp/dired-xdg-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun mp/backup-at-point nil
  "In dired, move the current file or directory at point to a new
backup with name `<current_name>_$(date --iso)'"
  (interactive)
  (let ((filename (dired-file-name-at-point)))
    (dired-rename-file filename (concat filename "_" (mp/get-date)) nil)
    (revert-buffer)))

(provide 'init-dired)
;;; init-dired.el ends here
