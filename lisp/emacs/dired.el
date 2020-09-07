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

(with-eval-after-load 'core-autoloads
  (require-package 'dired-ranger)
  (require-package 'dired-subtree))

(setq dired-listing-switches "-aFlv --group-directories-first")

;;;###autoload
(defun dired-initialize ()
  "Sets dired configurations."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;;;###autoload
(defun dired-hook-initialize ()
  "Sets dired configurations on hook."
  (dired-hide-details-mode 1))

;;;###autoload
(defer-loading
  :event
  dired-mode-hook
  :function
  dired-initialize
  dired-hook-initialize)

;;; dired.el ends here
