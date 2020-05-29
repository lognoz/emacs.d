;;; keybinding.el --- Dired Component Keybinding File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: dired

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
  :mode 'dired-mode-map
  :normal
    (kbd "<mouse-2>") 'dired-find-alternate-file
    (kbd "<s-tab>") 'dired-hide-details-mode
    (kbd "<M-tab>") 'dired-toogle-dotfile
    (kbd "<tab>") 'dired-subtree-toggle
    (kbd "<backtab>") 'dired-subtree-cycle
    (kbd "RET") 'dired-find-alternate-file
    "+" 'dired-create-directory
    "c" 'dired-do-compress-to
    "d" 'dired-flag-file-deletion
    "f" 'find-file
    "i" 'dired-maybe-insert-subdir
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "m" 'dired-mark
    "o" 'dired-find-file-other-window
    "q" 'quit-window
    "t" 'dired-toggle-marks
    "u" 'dired-unmark
    "x" 'dired-do-flagged-delete
    "y" 'dired-ranger-copy
    "p" 'dired-ranger-paste
    "C" 'dired-do-copy
    "D" 'dired-do-delete
    "F" 'find-file
    "K" 'dired-do-kill-lines
    "L" 'dired-do-redisplay
    "R" 'dired-do-rename
    "T" 'dired-do-touch
    "U" 'dired-unmark-all-marks
    "Z" 'dired-do-compress
    "%&" 'dired-flag-garbage-files
    "%C" 'dired-do-copy-regexp
    "%H" 'dired-do-hardlink-regexp
    "%R" 'dired-do-rename-regexp
    "%S" 'dired-do-symlink-regexp
    "%d" 'dired-flag-files-regexp
    "%g" 'dired-mark-files-containing-regexp
    "%l" 'dired-downcase
    "%m" 'dired-mark-files-regexp
    "%r" 'dired-do-rename-regexp
    "%u" 'dired-upcase
    "/f" 'dired-grep-file-name
    "/g" 'dired-grep-file-name-by-pattern
  :visual
    "m" 'dired-mark
    "d" 'dired-flag-file-deletion)
