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
  (prog-mode . electric-pair-mode))

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

(defun mp/eglot-ensure-if-in-project ()
  "Run eglot-ensure only if in a project."
  (when (project-current)
    (eglot-ensure)))

(use-package eglot
  :config
  (setq eglot-sync-connect 0)    ;; Don't block on connection
  (setq eglot-events-buffer-config '(:size 0))
  (setq eglot-inlay-hints-mode t)
  (setq eglot-autoshutdown t)
  (setq eglot-max-file-watches 5000)
  (add-to-list
   'eglot-server-programs
   `((elixir-mode elixir-ts-mode heex-ts-mode) ,(concat (getenv "HOME") "/elixir-ls/language_server.sh")))
  (add-to-list
   'eglot-server-programs
   '((tsx-ts-mode :language-id "typescriptreact") . ("rass" "--" "typescript-language-server" "--stdio" "--" "oxlint" "--lsp" "--" "oxfmt" "--lsp"))))

(use-package breadcrumb
  :after eglot
  :ensure t
  :init
  (add-hook 'eglot-managed-mode-hook #'breadcrumb-mode))

(use-package elixir-ts-mode
  :ensure t
  :hook ((heex-ts-mode . mp/eglot-ensure-if-in-project)
         (elixir-ts-mode . mp/eglot-ensure-if-in-project))
  :init
  (add-to-list 'load-path (concat user-emacs-directory "/site-elisp/flymake-credo"))
  (add-hook 'eglot-managed-mode-hook #'flymake-credo-load)
  (require 'flymake-credo)
  (setq flymake-credo-min-priority 1)
  (add-hook 'before-save-hook (lambda nil
                                (when (and (derived-mode-p '(elixir-mode))
                                           (eglot-current-server))
                                  (eglot-format-buffer)))))

(use-package goto-addr
  :ensure nil
  :hook
  (prog-mode . goto-address-mode))

(use-package lua-mode :ensure t)

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
         ("\\.exs\\'" . elixir-ts-mode)
         ("\\.lua\\'" . lua-ts-mode))
  :custom (treesit-font-lock-level 4)
  :init
  (dolist (mapping
           '((lua-mode . lua-ts-mode)
             (python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             (elixir-mode . elixir-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package typescript-ts-mode
  :ensure nil
  :hook (tsx-ts-mode . mp/eglot-ensure-if-in-project))

(use-package gdscript-mode
  :hook (gdscript-mode . mp/eglot-ensure-if-in-project))

(use-package vterm
  :ensure nil
  :hook (vterm-mode . (lambda ()
                        (setq show-trailing-whitespace nil))))

(use-package xml-format
  :ensure t)

(provide 'init-dev)
