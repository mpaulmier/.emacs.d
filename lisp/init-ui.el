;; Copyright (C) 2020  Matthias Paulmier

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

;; Note: In daemon mode, the font is not set properly in the very
;; first frame spawned. To work arount that, dismiss the first frame
;; and spawn a new one.

(require 'init-functions)

(use-package faces
  :ensure nil
  :bind ("C-c p" . mp/toggle-presentation-view)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-syntax '(green-strings))
  (modus-themes-prompts '(bold intense))
  (modus-themes-bold-constructs t)
  (modus-themes-completions 'opinionated)
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(accented borderless padded))
  (modus-themes-paren-match '(bold intense))
  (modus-themes-region '(no-extend accented))
  (modus-themes-slanted-constructs t)
  (modus-themes-hl-line '(intense))
  :custom-face
  (mode-line ((t (:underline nil))))
  :config
  (progn
    (load-theme 'modus-operandi t)
    (when (member "Iosevka" (font-family-list))
      (setq default-frame-alist '((font . "Iosevka"))))))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(provide 'init-ui)
