;;; config.el --- Dired Mode File

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

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-isearch-filenames 'dwim
      delete-by-moving-to-trash t
      dired-dwim-target t
      dired-listing-switches "-aFlv --group-directories-first")

(put 'dired-find-alternate-file 'disabled nil)

(evil-collection-define-key 'normal 'dired-mode-map
  [tab] 'dired-toogle-dotfile
  [mouse-2] 'dired-find-alternate-file
  (kbd "RET") 'dired-find-alternate-file)

(defun dired-toogle-dotfile ()
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
        (progn (revert-buffer)
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))
