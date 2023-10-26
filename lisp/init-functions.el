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

;; TODO: I think this actually exists in emacs, look it up and maybe remove this
(defun mp/join-line-one-space ()
  "A mix between join-line and just-one-space"
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (just-one-space -1)))

(defun mp/increment-number-at-point (&optional arg)
  (interactive)
  (save-excursion
    (save-match-data
      (let ((by (if arg arg 1)))
        (skip-chars-backward "0-9?-")
        (or (looking-at "-*[0-9]+")
            (error "No number at point"))
        (replace-match (number-to-string (+ by (string-to-number (match-string 0)))))))))

(defun mp/decrement-number-at-point (&optional arg)
  (interactive)
  (let ((by (if arg arg 1)))
    (mp/increment-number-at-point (- by))))

(defun mp/browse-emacs-conf-dir ()
  (interactive)
  (ido-find-file-in-dir (concat user-emacs-directory "lisp/")))

(defun mp/browse-org-dir ()
  (interactive)
  (ido-find-file-in-dir mp/org-directory))

(defun mp/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun mp/delete-trailing-whitespace-maybe ()
  "Run `delete-trailing-whitespace' when saving unless current mode
is in `no-dtw-modes'"
  (when (not (apply 'derived-mode-p mp/no-dtw-modes))
    (delete-trailing-whitespace)))

(defun mp/disable-stw-maybe ()
  "Set `show-trailing-whitespace' to nil for modes defined in
`no-stw-modes'"
  (when (apply 'derived-mode-p mp/no-stw-modes)
    (setq-local show-trailing-whitespace nil)))

(defun mp/save-executable-buffer ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (format "Made %s executable" buffer-file-name))))))

;; From https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun mp/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun mp/comint-clear ()
  (interactive)
  (let ((orig-ln (line-number-at-pos))
        (col (current-column))
        (cmd (progn (end-of-buffer)
                    (move-end-of-line nil)
                    (set-mark (point))
                    (move-beginning-of-line nil)
                    (buffer-substring (region-beginning) (region-end))))
        (after-ln (line-number-at-pos)))
    (delete-region (region-beginning) (region-end))
    (comint-clear-buffer)
    (insert cmd)
    (if (= orig-ln after-ln)
        (move-to-column col t)
      (move-beginning-of-line nil))))

(defun mp/dired-xdg-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file)))

(defun mp/get-date nil
  (interactive)
  (shell-command-to-string "echo -n $(date --iso)"))

(defun mp/backup-at-point nil
  "In dired, move the current file or directory at point to a new
backup with name `<current_name>_$(date --iso)'"
  (interactive)
  (let ((filename (dired-file-name-at-point)))
    (dired-rename-file filename (concat filename (mp/get-date)) nil)
    (revert-buffer)))

(defun org-copy-region-as-markdown ()
  "Copy the region (in Org) to the system clipboard as Markdown.
From: http://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard"
  (interactive)
  (if (use-region-p)
      (let* ((region
	      (buffer-substring-no-properties
		      (region-beginning)
		      (region-end)))
	     (markdown
	      (org-export-string-as region 'md t '(:with-toc nil))))
	(gui-set-selection 'CLIPBOARD markdown))))

(defun mp/toggle-presentation-view ()
  "Function to run when presenting emacs to users not familliar
with my setup With `C-u' prefix, change the theme
configuration (from light to dark and vice-versa)"
  (interactive)
  (if (equal '(4) current-prefix-arg)
      (modus-themes-toggle))
  (global-display-line-numbers-mode 'toggle)
  (global-hl-line-mode 'toggle))

(defun mp/get-file-name (filename &optional wilcards)
  "Get FILENAME without path"
  (interactive
   (find-file-read-args "Get filename: "
                        (confirm-nonexistent-file-or-buffer)))
  (replace-regexp-in-string ".*/\\(.*\\)" "\\1" filename))

(defun mp/insert-file-name (filename &optional wildcards)
  "Insert FILENAME at point without path

See `mp/insert-file-name'"
  (interactive
   (find-file-read-args "Insert filename: "
                        (confirm-nonexistent-file-or-buffer)))
  (insert (mp/get-file-name filename wildcards)))

(defun mp/sluggify-region (beg end)
  (interactive "r")
  (when (not mark-active)
    (cl-return))
  (save-excursion
    (replace-string " " "_" t beg end)
    (downcase-region beg end)))

(defun mp/beginning-of-line-or-indent ()
  "Function to put the cursor at the begining of the line or at the
indentation level. If the cursor position is anywhere but the
indentation level, send it to this position, otherwise, send it
to the begining of the line."
  (interactive)
  (let ((pos (point))
        (bol (save-excursion (move-beginning-of-line nil) (point)))
        (boi (save-excursion (back-to-indentation) (point))))
    (cond
     ((eq pos bol) (goto-char boi))
     ((eq pos boi) (goto-char bol))
     (t (goto-char boi)))))

(defun mp/vim-header ()
  (beginning-of-buffer))

;; Macros

(defmacro without-major-mode (&rest body)
  (declare (indent 0))
  `(progn
     (major-mode-suspend)
     ,@body
     (major-mode-restore)))

(provide 'init-functions)
