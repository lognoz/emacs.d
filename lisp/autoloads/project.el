;;; lisp/autoloads/project.el --- project autoloads -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: project

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
(defun git-remote-url ()
  "Return project remote origin url."
  (shell-command-to-string "git config --get remote.origin.url"))

;;;###autoload
(defun git-project-root (&optional filename)
  "Return project root directory.
The default FILENAME is the `buffer-file-name'."
  (setq filename (or filename (buffer-file-name)))
  (let ((dir (file-name-as-directory (file-name-directory filename))))
    (expand-file-name (locate-dominating-file dir ".git"))))

;;;###autoload
(defun project-relative-file (&optional filename)
  "Return project full path relative to the project root.
The default FILENAME is the `buffer-file-name'."
  (setq filename (or filename (buffer-file-name)))
  (let ((root (git-project-root filename))) 
    (if root
        (substring filename (length root))
      buffer-file-name)))

;;; project.el ends here
