;; direnv integration — picks up the env from nix develop / nix-direnv per project
(use-package envrc
  :ensure t
  :demand t
  :bind (:map envrc-mode-map
              ("C-c d" . envrc-command-map))
  :config
  (envrc-global-mode))

;; Inherit buffer-local env (set by envrc) when launching subprocesses such as
;; compile, shell-command, etc.
(use-package inheritenv
  :ensure t)

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(provide 'init-nix)
