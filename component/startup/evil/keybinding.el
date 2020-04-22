;;; keybinding.el --- Evil Component Keybinding File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: evil

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
  :mode 'evil-inner-text-objects-map
  :define
    "i" 'evil-indent-plus-i-indent
    "I" 'evil-indent-plus-i-indent-up
    "J" 'evil-indent-plus-i-indent-up-down)

(define-keybinding
  :mode 'evil-outer-text-objects-map
  :define
    "i" 'evil-indent-plus-a-indent
    "I" 'evil-indent-plus-a-indent-up
    "J" 'evil-indent-plus-a-indent-up-down)

(define-keybinding
  :mode 'evil-normal-state-map
  :define
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "=" 'evil-indent-line
    "<" 'evil-shift-left-line
    ">" 'evil-shift-right-line)

(define-keybinding
  :mode 'evil-visual-state-map
  :define
    "=" 'visual-indent
    "<" 'visual-shift-left
    ">" 'visual-shift-right)

;; Remove evil search and some motions.
(define-keybinding :mode 'evil-motion-state-map
                   :define "}" nil "{" nil "/" nil "*" nil)

;; Use shell-command function for '!' evil motions.
(define-keybinding :mode 'evil-motion-state-map
                   :define "!" 'shell-command)

;; Use goto-line function for ':' evil motions.
(define-keybinding :mode 'evil-motion-state-map
                   :define ":" 'goto-line)

;; Remove the vim way to reundo to keep native Emacs backward
;; search. Use C-? to reundo instead.
(define-keybinding :mode 'evil-normal-state-map
                   :define (kbd "C-r") nil)

;; Remove the vim way to start a macro. Use Emacs binding instead.
(define-keybinding :mode 'evil-normal-state-map
                   :define "q" nil)
