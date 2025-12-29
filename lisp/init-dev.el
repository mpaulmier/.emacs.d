(use-package cape
  :after eglot
  :ensure t
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; From corfu's wiki
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun mp/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       #'tempel-complete
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'mp/eglot-capf))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  (prog-mode . electric-layout-mode))

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

(add-hook 'before-save-hook #'mp/save-executable-buffer)

(use-package eglot
  :config
  (setq eglot-sync-connect nil)    ;; Don't block on connection
  (setq eglot-events-buffer-size 0)
  (add-to-list
   'eglot-server-programs
   `(elixir-mode ,(concat (getenv "HOME") "/elixir-ls/language_server.sh"))))

(use-package elixir-mode
  :ensure t
  :hook (elixir-mode . eglot-ensure)
  :init
  (add-to-list 'load-path (concat user-emacs-directory "/site-elisp/flymake-credo"))
  (add-hook 'eglot-managed-mode-hook #'flymake-credo-load)
  (require 'flymake-credo)
  (setq flymake-credo-min-priority 1)

  (setq auto-mode-alist
      (append
       '(("\\.ex\\'" . elixir-mode)
         ("\\.eex\\'" . elixir-mode))
       auto-mode-alist)))

(use-package goto-addr
  :ensure nil
  :hook
  (prog-mode . goto-address-mode))

(provide 'init-dev)
