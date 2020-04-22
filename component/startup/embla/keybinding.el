;;; keybinding.el --- Embla Component Keybinding File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: embla

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
    (kbd "<s-left>") 'winner-undo
    (kbd "<s-right>") 'winner-redo
    (kbd "C-'") 'er/expand-region
    (kbd "C-M-'") 'er/contract-region
    (kbd "C-S-z") 'undo-tree-redo
    (kbd "C-c C-e") 'eval-and-replace
    (kbd "C-c C-j") 'counsel-imenu
    (kbd "C-c p k") 'projectile-kill-buffers
    (kbd "C-c p u") 'browse-project-source
    (kbd "C-c p v") 'find-directory-local-variable-file
    (kbd "C-h f") 'find-function
    (kbd "C-h k") 'find-function-on-key
    (kbd "C-x ,") 'pop-local-mark-ring
    (kbd "C-x 4 t") 'open-terminal-other-window
    (kbd "C-x <down>") 'sort-lines
    (kbd "C-x =") 'align-regexp
    (kbd "C-x C-b") 'ibuffer
    (kbd "C-x C-r") 'revert-buffer
    (kbd "C-x p") 'find-project
    (kbd "C-x t") 'open-terminal
    (kbd "C-x w") 'elfeed
    (kbd "C-z") 'undo-tree-undo
    (kbd "M-f") 'find-file-on-cursor
    (kbd "s-1") 'delete-other-windows
    (kbd "s-2") 'split-window-below
    (kbd "s-3") 'split-window-right
    (kbd "s-k") 'kill-current-buffer
    (kbd "s-o") 'other-window)
