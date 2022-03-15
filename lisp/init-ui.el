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

;; TODO: Fix font on first frame?

(use-package faces
  :ensure nil
  :config
  (defun mp/set-theme (frame)
    (with-selected-frame frame
      (load-theme 'tsdh-light t)
      (when (member "Iosevka" (font-family-list))
        (setq default-frame-alist '((font . "Iosevka")))
        (set-face-attribute 'default nil :height 140)))
    (remove-hook 'after-make-frame-functions #'mp/set-theme))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'mp/set-theme)
    (progn
      (load-theme 'tsdh-light t)
      (when (member "Iosevka" (font-family-list))
        (setq default-frame-alist '((font . "Iosevka")))
        (set-face-attribute 'default nil :height 140)))))

(dolist (command
         '(scroll-up-command
           scroll-down-command
           recenter-top-bottom
           other-window
           previous-line
           next-line))
  (advice-add command :after #'mp/pulse-line))

(provide 'init-ui)
