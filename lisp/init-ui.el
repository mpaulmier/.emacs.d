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

(use-package faces
  :ensure nil
  :bind (("C-c t" . modus-themes-toggle)
         ("C-c p" . mp/toggle-presentation-view)))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-syntax '(green-strings))
  (modus-themes-prompts '(bold intense))
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((matches . (extrabold))
			                  (selection . (semibold accented))
			                  (popup . (accented intense))))
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(accented borderless padded))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-region '(no-extend accented))
  (modus-themes-slanted-constructs t)
  (modus-themes-hl-line '(intense))
  :config
  (setq modus-themes-operandi-color-overrides
	    '((bg-main . "#fffdf6")
          (bg-dim . "#fffde2")
          (bg-alt . "#fffdaa"))
	    modus-themes-vivendi-color-overrides
	    '((bg-main . "#25152a")
          (bg-dim . "#2a1930")
          (bg-alt . "#382443")))
  (load-theme 'modus-operandi t))

(use-package faces
  :ensure nil
  :bind ("C-c p" . mp/toggle-presentation-view)
  :custom-face
  (mode-line ((t (:underline nil))))
  :config
  (setq modus-themes-vivendi-color-overrides
	    '((bg-main . "#25152a")
          (bg-dim . "#2a1930")
          (bg-alt . "#382443")
          ;; more colours for `modus-vivendi'...
          ))
  (progn
    (when (member "Iosevka" (font-family-list))
      (setq default-frame-alist '((font . "Iosevka"))))))

(use-package nlinum
  :custom (nlinum-format " %d "))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(provide 'init-ui)
