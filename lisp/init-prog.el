;; Copyright (C) 2022  Matthias Paulmier

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; (use-package highlight-indent-guides
;;   :diminish highlight-indent-guides-mode
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-responsive 'top))

(use-package hideshow
  :diminish hs-minor-mode
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :init
  (setq eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package sideline
  :hook (eglot-managed-mode . sideline-mode)
  :diminish sideline-mode
  :init
  (use-package sideline-flymake)
  (setq sideline-backends-skip-current-line t
        sideline-order-left 'down
        sideline-order-right 'up
        sideline-format-left "%s   "
        sideline-format-right "   %s"
        sideline-priority 100
        sideline-display-backend-name t
        sideline-backends-right '(sideline-flymake)))

(use-package breadcrumb
  ;; :bind (:map breadcrumb-mode-map
  ;;             ("C-c j" . #'breadcrumb-jump))
  :init
  (breadcrumb-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dockerfile-mode)


;; Programming languages

(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook
  (clojure-mode . enable-paredit-mode)
  (clojure-mode . subword-mode))

(use-package cider
  :after clojure-mode
  :hook
  (cider-repl-mode . paredit-mode)
  :bind (:map clojure-mode-map
              ("C-x C-e" . cider-eval-last-sexp)
              ("C-c C-k" . cider-load-buffer)
              ("C-c C-c" . cider-eval-defun-at-point))
  :custom
  (cider-show-error-buffer t)
  (cider-auto-select-error-buffer t)
  (cider-repl-history-file "~/.config/emacs/cider-history")
  (cider-repl-wrap-history t))

(use-package dart-mode
  ;; Optional
  :hook ((dart-mode . flycheck-mode)
         (dart-mode . eglot-ensure)
         (dart-mode . flutter-test-mode)))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/opt/flutter/"))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . emmet-mode)))

(use-package js
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :hook (js-ts-mode . eglot-ensure)
  :custom (js-indent-level 2))

(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :init
  (setq show-paren-style 'paren))

(use-package slime
  :if (executable-find "sbcl")
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-contribs '(slime-fancy)))

(use-package lisp-mode
  :ensure nil
  :diminish lisp-mode
  :mode "\\.cl$\\|\\.lisp\\'")

(use-package lua-mode
  :ensure t
  :hook
  (lua-mode . electric-pair-mode)
  :custom ((lua-indent-level 4)
           (lua-indent-nested-block-content-align nil)))

(use-package python
  :ensure nil
  :mode ("\\.py" . python-ts-mode)
  :hook
  (python-ts-mode . eglot-ensure)
  :bind (:map python-ts-mode-map
              ("M-<right>" . python-indent-shift-right)
              ("M-<left>" . python-indent-shift-left)
              ("C-c C-t d" . python-skeleton-method))
  :custom (python-check-command "flake8 --max-complexity 15 --color never")
  :config
  (python-skeleton-define method nil
    "Function name: "
    "@classmethod" \n
    "def " str "(cls, " ("Parameter, %s: "
                  (unless (equal ?\( (char-before)) ", ")
                  str) "):" \n
                  "\"\"\"" - "\"\"\"" \n
                  > _ \n))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

(use-package css-mode
  :ensure nil
  :init
  (when (not (null mp/tree-sitter-dir))
    (add-hook 'css-mode 'css-ts-mode)))

(use-package shell
  :ensure nil
    :init
  (when (not (null mp/tree-sitter-dir))
    (add-hook 'bash-mode 'bash-ts-mode)))

(provide 'init-prog)
