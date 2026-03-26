;;; init-project.el --- Project.el configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for project.el and related extensions.

;;; Code:

(defun mp/project-query-replace-regexp (from to &optional file-regexp)
  "Query-replace FROM with TO in project files matching FILE-REGEXP.
Like `project-query-replace-regexp', but with an additional FILE-REGEXP
argument to filter which files to operate on by their filename.

If FILE-REGEXP is nil or empty, operates on all project files."
  (interactive
   (let ((query-replace-args (query-replace-read-args "Query replace (regexp)" t t)))
     (list (nth 0 query-replace-args)
           (nth 1 query-replace-args)
           (read-regexp "File name regexp (empty for all): " nil))))
  (require 'project)
  (let* ((project (project-current t))
         (files (project-files project))
         (filtered-files (if (and file-regexp (not (string-empty-p file-regexp)))
                             (seq-filter (lambda (f)
                                           (string-match-p file-regexp (file-name-nondirectory f)))
                                         files)
                           files)))
    (if (null filtered-files)
        (user-error "No files match the pattern")
      (fileloop-initialize-replace from to filtered-files 'default)
      (fileloop-continue))))

(use-package project
  :ensure nil
  :config
  (define-key project-prefix-map "r" #'mp/project-query-replace-regexp))

(provide 'init-project)
;;; init-project.el ends here
