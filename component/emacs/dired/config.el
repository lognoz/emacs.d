;;; config.el --- Dired Component Config File

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

;;; Contextual core variables.

(defvar dired-emacs-loader-hooks '(dired-mode-hook)
  "The hook that load dired emacs module.")

;;; Internal core functions.

(defun dired-init-dired ()
  "Initialization of dired mode."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-isearch-filenames 'dwim
        dired-auto-revert-buffer t
        delete-by-moving-to-trash t
        dired-dwim-target t))

(defun dired-init-dired-x ()
  "Initialization of dired-x package."
  (dired-hide-details-mode 1))

(defun dired-define-keybinding ()
  "Define keybindings related to dired module. For more
information, see the documentation."
  (define-keybinding
    :mode
      'dired-mode-map
    :normal
      (kbd "RET")       'dired-find-alternate-file
      (kbd "<mouse-2>") 'dired-find-alternate-file
      (kbd "<s-tab>")   'dired-hide-details-mode
      (kbd "<M-tab>")   'dired-toogle-dotfile
      (kbd "<tab>")     'dired-subtree-toggle
      (kbd "<backtab>") 'dired-subtree-cycle
      (kbd "C-c C-r")   'dired-rsync

      ;; Mouvement
      "q" 'quit-window
      "j" 'dired-next-line
      "k" 'dired-previous-line

      ;; Edition
      "C" 'dired-do-copy
      "R" 'dired-do-rename
      "T" 'dired-do-touch
      "D" 'dired-do-delete
      "x" 'dired-do-flagged-delete
      "+" 'dired-create-directory

      ;; Compression
      "Z" 'dired-do-compress
      "c" 'dired-do-compress-to))
