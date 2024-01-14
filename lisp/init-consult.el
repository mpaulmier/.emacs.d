;; Copyright (C) 2023  Matthias Paulmier

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

(use-package consult
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

(provide 'init-consult)
