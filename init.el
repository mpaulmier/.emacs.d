;; Set lexical-binding to true by default
(setq-default lexical-binding t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(put 'narrow-to-region 'disabled nil)

(defun mp/beginning-of-line-or-indent ()
  "Put the cursor at the begining of the line or at the
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

(global-set-key (kbd "C-a") #'mp/beginning-of-line-or-indent)

(defconst mp/emacs-tmp-dir
  (format "%s%s%s/" temporary-file-directory "emacs" (user-uid))
  "Temporary files go here")

(defconst mp/custom-file
  (concat user-emacs-directory "custom.el")
  "Recipient for modifications made through `M-x customize'")

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
(define-key global-map (kbd "<f5>") #'revert-buffer-quick)

(setq inhibit-startup-message t
      auto-save-default nil
      backup-directory-alist `((".*" . ,mp/emacs-tmp-dir))
      create-lockfiles nil
      custom-file mp/custom-file
      redisplay-dont-pause t
      delete-by-moving-to-trash t
      confirm-kill-emacs 'yes-or-no-p
      mouse-yank-at-point t
      help-window-select t
      select-enable-clipboard t
      window-combination-resize t
      ring-bell-function nil
      visible-bell nil
      scroll-conservatively most-positive-fixnum
      display-time-interval 10)

(setq-default show-trailing-whitespace t
              truncate-lines t
              major-mode 'text-mode
              fill-column 80
              display-fill-column-indicator t
              display-fill-column-indicator-character "│"
              tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'complete
              sort-fold-case t)

(fset 'yes-or-no-p 'y-or-n-p)

(use-package ns-win
  :ensure nil
  :custom
  (mac-right-option-modifier "alt")
  :init
  (global-set-key [kp-delete] 'delete-char))

;; AI

;;; required by claude code
(use-package vterm :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup))

(require 'init-package)
(require 'init-dev)
(require 'init-completion)
(require 'init-vcs)
(require 'init-ui)

(setq debug-on-error t)
