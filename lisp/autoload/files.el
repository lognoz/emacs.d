;;; lisp/autoload/files.el --- files autoload -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: files autoload

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains somes helpers functions to manage files and
;; directories.

;;; Code:

;;;###autoload
(defun directory-name (path)
  "Return the directory name of a reference PATH."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory path))))

;;;###autoload
(defmacro fetch-subdirectories (path &rest body)
  "Fetch subdirectories located in the given PATH."
  `(dolist (p (directories-list ,path))
     (let ((directory (directory-name p)))
       ,@body)))

;;;###autoload
(defun subdirectories (path)
  "Return subdirectories located in a given PATH."
  (let ((directories))
    (dolist (f (directory-files path))
      (let ((path (concat path f)))
        (when (and (file-directory-p path)
                   (not (equal f "."))
                   (not (equal f "..")))
          (push (file-name-as-directory path) directories))))
    directories))
