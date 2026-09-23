(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(defvar mp/fixed-width-font "Iosevka Nerd Font Mono"
  "The font to use for monospaced (fixed width) text.")

;; Variable pitch doesn't work for now, I'll stick with a mono font and see if one works for me later
(defvar mp/variable-width-font "Garamontio"
  "The font to use for variable-pitch (document) text.")

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t (:underline nil)))))

(use-package ef-themes
  :ensure t
  :init
  (load-theme 'ef-day))

;; (use-package nano
;;   :vc (:url "https://github.com/rougier/nano-emacs" :rev :newest)
;;   :custom
;;   (nano-font-family-monospaced mp/fixed-width-font)
;;   (nano-font-family-proportional nil)
;;   (nano-font-size 22)
;;   :init
;;   (progn
;;     (require 'nano-layout)
;;     (require 'nano-faces)
;;     (require 'nano-theme)
;;     ;; (require 'nano-theme-dark)
;;     (require 'nano-theme-light)
;;     (nano-theme-set-light)
;;     (call-interactively 'nano-refresh-theme)
;;     (require 'nano-defaults)
;;     (require 'nano-session)
;;     (require 'nano-modeline)
;;     (let ((inhibit-message t))
;;       (message "Welcome to GNU Emacs / N Λ N O edition")
;;       (message (format "Initialization time: %s" (emacs-init-time))))
;;     (require 'nano-splash)
;;     (require 'nano-help)))

(use-package display-line-numbers
  :ensure nil
  :hook (after-init . global-display-line-numbers-mode)
  :custom-face
  (line-number-current-line ((t (:font-family ,mp/fixed-width-font
                                              :foreground ,(face-attribute 'error :foreground nil 'default)
                                              :background ,(face-attribute 'highlight :background nil 'default)))))
  (line-number ((t (:font-family ,mp/fixed-width-font))))
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

(use-package emacs
  :ensure nil
  :init
  (column-number-mode)
  :config
  (global-set-key
   (kbd "C-c r")
   #'(lambda nil
       "Replace string starting from the beggining of the current buffer (visible only)"
       (interactive)
       (save-excursion
         (beginning-of-buffer)
         (call-interactively #'replace-string)))))

;; Show colors in compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Whitespace

(defvar mp/no-stw-modes '(eat-mode comint-mode markdown-mode special-mode)
  "Modes where `show-trailing-whitespace' should be disabled.")

(defun mp/disable-stw-maybe ()
  "Set `show-trailing-whitespace' to nil for modes defined in
`no-stw-modes'"
  (when (derived-mode-p mp/no-stw-modes)
    (setq-local show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook #'mp/disable-stw-maybe)

(defun mp/delete-trailing-whitespace-maybe ()
  "Run `delete-trailing-whitespace' when saving unless current mode
is in `no-dtw-modes'"
  (when (not (apply 'derived-mode-p mp/no-dtw-modes))
    (delete-trailing-whitespace)))

;;; Fullscreen

(defun mp/show-time-for-fullscreen (frame)
  "Show the time in the modeline when the FRAME becomes full screen.
From https://emacs.ch/@bram85/112463005253079332"
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (if (memq fullscreen '(fullscreen fullboth))
        (display-time-mode 1)
      (display-time-mode -1))))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'init-ui)
