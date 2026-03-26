;;; init-meta-project.el --- Meta-project backend for project.el -*- lexical-binding: t; -*-

;; Author: Matthias Paulmier
;; Keywords: project, convenience

;;; Commentary:

;; This package provides a custom project.el backend that treats a parent
;; directory containing multiple sub-projects as a single unified project.
;;
;; This is useful when you have a directory structure like:
;;
;;   ~/work/
;;     ├── .meta-project        <- marker file
;;     ├── .dir-locals.el       <- optional exclusions
;;     ├── frontend/
;;     │   └── .git/
;;     ├── backend/
;;     │   └── .git/
;;     └── archived/            <- can be excluded
;;
;; Without this package, project.el would treat each sub-directory with a
;; .git folder as a separate project.  With this package, the entire ~/work/
;; directory becomes a single project.
;;
;;; Setup:
;;
;; 1. Create an empty marker file in the parent directory:
;;
;;      touch ~/work/.meta-project
;;
;; 2. (Optional) Create a .dir-locals.el to exclude subdirectories:
;;
;;      ((nil . ((mp/meta-project-exclude . ("archived" "experiments")))))
;;
;; 3. Require this module in your config:
;;
;;      (require 'init-meta-project)
;;
;;; How it works:
;;
;; The package registers `mp/meta-project-find' in `project-find-functions'
;; with priority -90, so it runs before the default VC backend (priority 0).
;; When a .meta-project marker is found, it returns a project of type 'meta.
;;
;; The `project-files' method uses external tools (fd, rg, or find) to list
;; files efficiently, respecting any exclusions defined in .dir-locals.el.

;;; Code:

(require 'project)
(require 'cl-lib)

;;; Customization

(defgroup meta-project nil
  "Meta-project backend for project.el."
  :group 'project
  :prefix "mp/meta-project-")

(defcustom mp/meta-project-marker ".meta-project"
  "Marker file to identify meta-project roots.
Create an empty file with this name in the directory you want to
treat as a meta-project root."
  :type 'string
  :group 'meta-project)

;;; Variables

(defvar-local mp/meta-project-exclude nil
  "List of subdirectories to exclude from this meta-project.
Set via .dir-locals.el in the meta-project root.

Example .dir-locals.el:
  ((nil . ((mp/meta-project-exclude . (\"archived\" \"experiments\")))))")

;; Mark as safe for dir-locals when value is a list of strings
(put 'mp/meta-project-exclude 'safe-local-variable
     (lambda (v) (and (listp v) (cl-every #'stringp v))))

;;; Project detection

(defun mp/meta-project--inside-sub-project-p (dir root)
  "Return non-nil if DIR is inside a VCS sub-project under ROOT.
Checks whether a .git directory exists between DIR and ROOT,
meaning a more specific project should take precedence."
  (let ((dir (expand-file-name dir))
        (root (expand-file-name root)))
    (and (not (file-equal-p dir root))
         (locate-dominating-file
          dir
          (lambda (d)
            (and (not (file-equal-p d root))
                 (file-exists-p (expand-file-name ".git" d))))))))

(defun mp/meta-project-find (dir)
  "Find a meta-project root by looking for the marker file.
DIR is the directory to start searching from.
Returns a cons cell (meta . ROOT-DIR) if found, nil otherwise.
If DIR is inside a VCS sub-project, return nil so the VC backend
handles it instead."
  (when-let ((root (locate-dominating-file dir mp/meta-project-marker)))
    (let ((root (file-name-as-directory root)))
      ;; Only claim this directory if it's not inside a VCS sub-project.
      ;; If there's a .git between DIR and the meta-project root, the VC
      ;; backend should handle it.
      (unless (mp/meta-project--inside-sub-project-p dir root)
        (cons 'meta root)))))

;;; Project methods

(cl-defmethod project-root ((project (head meta)))
  "Return the root directory of a meta PROJECT."
  (cdr project))

(defun mp/meta-project-get-exclusions (root)
  "Get the exclusion list from .dir-locals.el in ROOT.
Returns the value of `mp/meta-project-exclude' if defined."
  (let ((dir-locals-file (expand-file-name ".dir-locals.el" root)))
    (when (file-exists-p dir-locals-file)
      (with-temp-buffer
        (setq default-directory root)
        (hack-dir-local-variables-non-file-buffer)
        mp/meta-project-exclude))))

;;; File listing commands

(defun mp/meta-project--build-fd-command (exclusions)
  "Build fd command string with EXCLUSIONS.
fd is the preferred tool for its speed and sensible defaults."
  (concat "fd --type f --hidden --exclude .git"
          (mapconcat (lambda (dir) (concat " --exclude " (shell-quote-argument dir)))
                     exclusions "")))

(defun mp/meta-project--build-rg-command (exclusions)
  "Build rg command string with EXCLUSIONS.
ripgrep is used as a fallback when fd is not available."
  (concat "rg --files --hidden --glob '!.git'"
          (mapconcat (lambda (dir) (concat " --glob '!" dir "'"))
                     exclusions "")))

(defun mp/meta-project--build-find-command (exclusions)
  "Build find command string with EXCLUSIONS.
Used as a last resort when neither fd nor rg is available."
  (concat "find . -type f -not -path '*/.git/*'"
          (mapconcat (lambda (dir) (concat " -not -path '*/" dir "/*'"))
                     exclusions "")
          " | cut -c3-"))

(cl-defmethod project-files ((project (head meta)) &optional _dirs)
  "Return a list of all files in meta PROJECT.
Uses fd, rg, or find (in order of preference) to list files.
Respects exclusions defined in `mp/meta-project-exclude'."
  (let* ((root (project-root project))
         (exclusions (mp/meta-project-get-exclusions root))
         (default-directory root)
         (cmd (cond
               ((executable-find "fd")
                (mp/meta-project--build-fd-command exclusions))
               ((executable-find "rg")
                (mp/meta-project--build-rg-command exclusions))
               (t
                (mp/meta-project--build-find-command exclusions)))))
    (mapcar (lambda (f) (expand-file-name f root))
            (split-string (shell-command-to-string cmd) "\n" t " "))))

(cl-defmethod project-ignores ((project (head meta)) _dir)
  "Return list of patterns to ignore in meta PROJECT.
Combines common ignore patterns with user exclusions from
`mp/meta-project-exclude'."
  (let ((exclusions (mp/meta-project-get-exclusions (project-root project))))
    (append '(".git/" "node_modules/" "_build/" "deps/" "__pycache__/" "*.elc" "*.pyc" "*.beam")
            (mapcar (lambda (dir) (concat dir "/")) exclusions))))

;;; Commands

(defun mp/meta-project-switch ()
  "Switch to the meta-project containing the current directory.
Opens the project dispatch menu at the meta-project root,
allowing you to run any `C-x p' command in the meta-project context."
  (interactive)
  (if-let ((root (locate-dominating-file default-directory mp/meta-project-marker)))
      (project-switch-project (file-name-as-directory root))
    (user-error "No meta-project found above %s" default-directory)))

;;; Registration

;; Add with high priority (-90) so it's checked before the VC backend (0)
(add-hook 'project-find-functions #'mp/meta-project-find -90)

(define-key project-prefix-map "M" #'mp/meta-project-switch)

(provide 'init-meta-project)
;;; init-meta-project.el ends here
