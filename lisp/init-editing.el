;;; init-editing.el --- Editing commands and text manipulation -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defun mp/beginning-of-line-or-indent ()
  "Toggle cursor between beginning of line and indentation level."
  (interactive)
  (let ((pos (point))
        (bol (save-excursion (move-beginning-of-line nil) (point)))
        (boi (save-excursion (back-to-indentation) (point))))
    (cond
     ((eq pos bol) (goto-char boi))
     ((eq pos boi) (goto-char bol))
     (t (goto-char boi)))))

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

(defun mp/sluggify-region (beg end)
  (interactive "r")
  (when (not mark-active)
    (cl-return))
  (save-excursion
    (replace-string " " "_" t beg end)
    (downcase-region beg end)))

(defun mp/insert-semi-col ()
  (interactive)
  (end-of-line)
  (insert ";")
  (newline nil t))

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

(defmacro without-major-mode (&rest body)
  (declare (indent 0))
  `(progn
     (major-mode-suspend)
     ,@body
     (major-mode-restore)))

(global-set-key (kbd "C-a") #'mp/beginning-of-line-or-indent)

(provide 'init-editing)
;;; init-editing.el ends here
