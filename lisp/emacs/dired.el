;;; lisp/emacs/dired.el --- dired configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: dired

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

;;;###autoload
(boot-packages 'dired-ranger
               'dired-subtree)

;;;###autoload
(advice dired-mode-hook
        setup-dired
        dired-hide-details-mode)

;;;###autoload
(setq dired-listing-switches "-aFlv --group-directories-first")

;;;###autoload
(defun setup-dired ()
  "Setup dired configurations."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "RET") 'dired-find-alternate-file
    (kbd "<mouse-2>") 'dired-find-alternate-file
    (kbd "<s-tab>") 'dired-hide-details-mode
    (kbd "<M-tab>") 'dired-toggle-dotfile
    (kbd "<tab>") 'dired-subtree-toggle
    "o"  'dired-find-file-other-window
    "/f" 'dired-grep-file-name
    "/g" 'dired-grep-file-name-by-pattern))

(defun dired-toggle-dotfile ()
  "Switch visibility of dotfiles lines."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun dired-grep-file-name (name)
  "Search files in directory by name."
  (interactive "sFind-name (filename wildcard): ")
  (find-name-dired default-directory name))

(defun dired-grep-file-name-by-pattern (pattern)
  "Search files in directory by PATTERN."
  (interactive "sFind-grep (grep regexp): ")
  (find-grep-dired default-directory pattern))

;;; dired.el ends here
