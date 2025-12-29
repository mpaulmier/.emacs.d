(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t (:underline nil))))
  :config
  (let ((mono-spaced-font "Iosevka Term")
	(proportionately-spaced-font "Sans"))
    (set-face-attribute 'default nil :family mono-spaced-font :height 120)
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))
  (load-theme 'tango))

(use-package display-line-numbers
  :ensure nil
  :hook (after-init . global-display-line-numbers-mode)
  :custom-face (line-number-current-line ((t (:foreground ,(face-attribute 'error :foreground nil 'default)
                                              :background ,(face-attribute 'highlight :background nil 'default)))))
  :custom (display-line-numbers-width-start t))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-separator " → "))

(use-package ibuffer
  :ensure nil
  :init
  (fset 'list-buffers 'ibuffer))

(provide 'init-ui)
