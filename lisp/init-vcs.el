(use-package magit
  :if (executable-find "git")
  :ensure t
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
  (add-to-list 'load-path (concat user-emacs-directory "/site-elisp/magit-todos"))
  (dolist (fun '(magit-status magit-project-status))
    (advice-add fun :around #'magit-status--ad-around))
  :config
  (require 'magit-todos)
  (magit-todos-mode 1)
  ;; full screen magit-status
  ;; From http://whattheemacsd.com/setup-magit.el-01.html
  (defun magit-status--ad-around (old-func &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply old-func args)
    (delete-other-windows))

  (defun mp/magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package magit
  :if (eq system-type 'darwin)
  :init
  ;; Small hack for macos, don't do this maybe
  (defadvice server-ensure-safe-dir (around
                                     my-around-server-ensure-safe-dir
                                     activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it))
  (let ((git-exe-path "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))
    (when (file-exists-p git-exe-path)
      (setq magit-git-executable git-exe-path))))


(provide 'init-vcs)
