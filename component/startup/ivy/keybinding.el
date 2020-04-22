;;; keybinding.el --- Ivy Component Keybinding File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: ivy counsel

;; This file is not part of GNU Emacs.

;; This Emacs config is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This Emacs config is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this Emacs config. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(define-keybinding
  :mode 'embla-mode-map
  :define
    (kbd "M-x") 'counsel-M-x
    (kbd "M-y") 'counsel-yank-pop
    (kbd "C-x d") 'counsel-dired
    (kbd "C-x f") 'counsel-recentf
    (kbd "C-x C-f") 'counsel-find-file
    (kbd "C-c C-j") 'counsel-imenu
    (kbd "C-x r l") 'counsel-bookmark)

(define-keybinding
  :mode 'ivy-minibuffer-map
  :define
    (kbd "TAB") 'ivy-alt-done)
