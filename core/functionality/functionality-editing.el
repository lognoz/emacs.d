;;; functionality-editing.el --- Editing Function File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: functionality editing content

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

;;;###autoload
(defun get-file-content (path)
  "Return the contents of filename."
  (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;;###autoload
(defun replace-in-file (path replacement)
  "Replace variables in file."
  (with-temp-file path
    (insert-file-contents-literally path)
    (mapc (lambda (entry)
      (setq entry (eval entry))
      (goto-char 0)
      (while (search-forward (car entry) nil t)
        (replace-match (cdr entry))))
      replacement)))

;;;###autoload
(defun replace-in-string (string replacement)
  "Replace variables in string."
  (mapc (lambda (entry)
    (setq entry (eval entry))
    (setq string
      (replace-regexp-in-string
        (car entry) (cdr entry) string nil 'literal)))
    replacement)
  string)

;;;###autoload
(defun recursive-directories (path)
  "Return all directories in a specific path."
  (split-string
    (shell-command-to-string
      (concat "find " path " -type d"))
    "\n" t))
