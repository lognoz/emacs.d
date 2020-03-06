;;; core-keybinding.el --- Keybinding Initialization File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: keybinding

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

;; Keybinding align a region.
(global-set-key (kbd "C-x =") 'align-regexp)

;; Keybinding to sort lines.
;; If you want to execute descending sort, just execute C-u C-x
;; <down>.
(global-set-key (kbd "C-x <down>") 'sort-lines)

;; Keybinding to show ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Keybinding eval statement and replace.
;; Ex (+ 1 2 3) = 6
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Keybinding to use winner mode easily.
(global-set-key (kbd "<s-right>") 'winner-redo)
(global-set-key (kbd "<s-left>") 'winner-undo)

;; Open terminal in right tab.
(global-set-key (kbd "C-x C-t") 'open-terminal)
(global-set-key (kbd "C-x 4 t") 'open-terminal-other-window)

;; Define better emacs help keybindings.
(global-set-key (kbd "C-h f") 'find-function)
(global-set-key (kbd "C-h k") 'find-function-on-key)

;; Keybindings fo undo and redo.
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

;; Keybinding to occur in multiple buffer.
(global-set-key (kbd "M-s M-o") 'multi-occur)

;; Keybinding to show counsel imenu.
(global-set-key (kbd "C-c C-j") 'counsel-imenu)

(provide 'core-keybinding)
