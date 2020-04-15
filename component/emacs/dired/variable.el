;;; variable.el --- Dired Component Variable File

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

(defvar dired-emacs-loader-hooks '(dired-mode-hook)
  "The hook that load dired emacs module.")

(defvar dired-emacs-keybindings
  (define-keybinding
    :mode 'dired-mode-map
    :normal
      (kbd "<mouse-2>") 'dired-find-alternate-file
      (kbd "<s-tab>") 'dired-hide-details-mode
      (kbd "<M-tab>") 'dired-toogle-dotfile
      (kbd "<tab>") 'dired-subtree-toggle
      (kbd "<backtab>") 'dired-subtree-cycle
      (kbd "C-c C-r") 'dired-rsync
      (kbd "RET") 'dired-find-alternate-file
      "q" 'quit-window
      "o" 'dired-mouse-find-file-other-window
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "C" 'dired-do-copy
      "R" 'dired-do-rename
      "T" 'dired-do-touch
      "D" 'dired-do-delete
      "K" 'dired-do-kill-lines
      "x" 'dired-do-flagged-delete
      "+" 'dired-create-directory
      "Z" 'dired-do-compress
      "c" 'dired-do-compress-to
      "t" 'dired-toggle-marks
      "m" 'dired-mark
      "%u" 'dired-upcase
      "%l" 'dired-downcase
      "%d" 'dired-flag-files-regexp
      "%g" 'dired-mark-files-containing-regexp
      "%m" 'dired-mark-files-regexp
      "%r" 'dired-do-rename-regexp
      "%C" 'dired-do-copy-regexp
      "%H" 'dired-do-hardlink-regexp
      "%R" 'dired-do-rename-regexp
      "%S" 'dired-do-symlink-regexp
      "%&" 'dired-flag-garbage-files
    :visual
      "m" 'dired-mark-selection))
