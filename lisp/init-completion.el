;; In mini buffer completions

(use-package vertico
  :ensure t
  :custom (vertico-cycle t)
  :hook (after-init . vertico-mode)
  :config
  ;; Ring bell on cycle
  (advice-add #'vertico-next
              :around
              #'(lambda (origin &rest args)
                  (let ((beg-index vertico--index))
                    (apply origin args)
                    (if (not (eq 1 (abs (- beg-index vertico--index))))
                        (let ((visible-bell t))
                          (ding)))))))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :ensure t
  :bind (("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("C-S-s" . consult-ripgrep)
         ([remap pop-mark] . consult-mark)
         ([remap pop-global-mark] . consult-global-mark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap apropos-command] . consult-apropos))
  :custom
  (consult-preview-key 'any)
  :config
  (setq consult-project-root-function
        #'(lambda ()
            (when-let (project (project-current))
              (car (project-roots project))))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; In buffer completion

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  ;; Enable auto completion, configure delay, trigger and quitting
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-trigger "."
        corfu-quit-no-match 'separator))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))



;; Completion style

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t))

(provide 'init-completion)
