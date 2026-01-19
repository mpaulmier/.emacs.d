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
   `(elixir-mode ,(concat (getenv "HOME") "/elixir-ls/language_server.sh")))
  (add-to-list
   'eglot-server-programs
   `(tsx-ts-mode "rass" "--" "typescript-language-server" "--stdio" "--" "vscode-eslint-language-server" "--stdio")))

(use-package breadcrumb
  :after eglot
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook #'breadcrumb-mode))

(use-package elixir-ts-mode
  :ensure t
  :hook (elixir-ts-mode . eglot-ensure)
  :init
  (add-to-list 'load-path (concat user-emacs-directory "/site-elisp/flymake-credo"))
  (add-hook 'eglot-managed-mode-hook #'flymake-credo-load)
  (require 'flymake-credo)
  (setq flymake-credo-min-priority 1))

(use-package goto-addr
  :ensure nil
  :hook
  (prog-mode . goto-address-mode))

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . tsx-ts-mode)
         ("\\.mjs\\'" . tsx-ts-mode)
         ("\\.mts\\'" . tsx-ts-mode)
         ("\\.cjs\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.ex\\'" . elixir-ts-mode)
         ("\\.eex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode))
  :custom (treesit-font-lock-level 4)
  :init
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (bash-mode . bash-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             (elixir-mode . elixir-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))


(use-package claude-code-ide
  :ensure t)

(provide 'init-dev)
