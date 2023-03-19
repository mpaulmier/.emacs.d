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

(require 'init-functions)

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-paren-match '(bold))
  (modus-themes-region '(no-extend))
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package faces
  :ensure nil
  :bind (("C-c t" . modus-themes-toggle)
         ("C-c p" . mp/toggle-presentation-view))
  :custom-face
  (mode-line ((t (:underline nil))))
  :config
  (when (member "Iosevka" (font-family-list))
    (setq default-frame-alist '((font . "Iosevka"))))
  (load-theme 'modus-operandi-tinted))

(use-package hl-line
  :ensure nil
  :custom-face
  (hl-line ((t (:extend nil))))

  :init
  (setq-default hl-line-range-function #'mp/hl-line-range-function)
  :config
  (global-hl-line-mode))

(use-package simple
  :ensure nil
  :custom (nlinum-format " %d ")
  (scroll-margin 15))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative))

(provide 'init-ui)
