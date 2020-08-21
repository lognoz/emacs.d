;;; lisp/editor/evil.el --- evil configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: evil vim

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require-package 'evil
                 'evil-magit
                 'evil-indent-plus
                 'evil-smartparens
                 'evil-surround)

;;; --- Evil configuration

(setq evil-toggle-key "C-`"
      evil-want-keybinding nil)

(global-evil-surround-mode t)
(evil-mode t)


;;; --- Evil keybindings

(set-keybindings
  :map evil-inner-text-objects-map
  "i"  evil-indent-plus-i-indent
  "I"  evil-indent-plus-i-indent-up
  "J"  evil-indent-plus-i-indent-up-down

  :map evil-outer-text-objects-map
  "i"  evil-indent-plus-a-indent
  "I"  evil-indent-plus-a-indent-up
  "J"  evil-indent-plus-a-indent-up-down

  :map evil-normal-state-map
  "j"  evil-next-visual-line
  "k"  evil-previous-visual-line
  "="  evil-indent-line
  "<"  evil-shift-left-line
  ">"  evil-shift-right-line

  :map evil-motion-state-map
  "!"  shell-command
  ":"  goto-line)

;; Remove evil search and some motions.
(unset-keybindings evil-motion-state-map "{" "}" "/" "*")

;; Remove the vim way to reundo to keep native Emacs backward
;; search. Use C-? to reundo instead.
(unset-keybindings evil-normal-state-map "C-r")

;; Remove the vim way to start a macro. Use Emacs binding instead.
(unset-keybindings evil-normal-state-map "q")
