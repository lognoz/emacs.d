;;; config.el --- Ibuffer Config File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: ibuffer

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

;;; Contextual component variables.

(defvar ibuffer-emacs-loader-hooks '(ibuffer-mode-hook)
  "The hook that load ibuffer emacs module.")

;;; Internal component functions.

(defun ibuffer-init-ibuffer ()
  "Initialization of ibuffer mode."
  (unless (equal ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(defun ibuffer-init-ibuffer-projectile ()
  "Initialization of ibuffer projectile."
  (ibuffer-projectile-set-filter-groups))

(defun ibuffer-init-evil ()
  "Initialization of inital evil state."
  (evil-set-initial-state 'ibuffer-mode 'normal))

(defun ibuffer-define-keybinding ()
  "Define keybindings related to ibuffer module. For more
information, see the documentation."
  (define-keybinding :mode 'ibuffer-mode-map :normal
    "d"  'ibuffer-mark-for-delete
    "x"  'ibuffer-do-kill-on-deletion-marks
    "A"  'ibuffer-do-view
    "D"  'ibuffer-do-delete
    "E"  'ibuffer-do-eval
    "F"  'ibuffer-do-shell-command-file
    "I"  'ibuffer-do-query-replace-regexp
    "H"  'ibuffer-do-view-other-frame
    "N"  'ibuffer-do-shell-command-pipe-replace
    "M"  'ibuffer-do-toggle-modified
    "O"  'ibuffer-do-occur
    "P"  'ibuffer-do-print
    "Q"  'ibuffer-do-query-replace
    "R"  'ibuffer-do-rename-uniquely
    "S"  'ibuffer-do-save
    "T"  'ibuffer-do-toggle-read-only
    "r"  'ibuffer-do-replace-regexp
    "V"  'ibuffer-do-revert
    "W"  'ibuffer-do-view-and-eval
    "m"  'ibuffer-mark-forward
    "~"  'ibuffer-toggle-marks
    "u"  'ibuffer-unmark-forward
    "U"  'ibuffer-unmark-all-marks
    "**" 'ibuffer-unmark-all
    "*c" 'ibuffer-change-marks
    "*M" 'ibuffer-mark-by-mode
    "*m" 'ibuffer-mark-modified-buffers
    "*u" 'ibuffer-mark-unsaved-buffers
    "*s" 'ibuffer-mark-special-buffers
    "*r" 'ibuffer-mark-read-only-buffers
    "*/" 'ibuffer-mark-dired-buffers
    "*e" 'ibuffer-mark-dissociated-buffers
    "*h" 'ibuffer-mark-help-buffers
    "*z" 'ibuffer-mark-compressed-file-buffers
    "."  'ibuffer-mark-old-buffers
    "q"  'quit-window))
