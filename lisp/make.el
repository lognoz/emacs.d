;;; lisp/make.el --- make functions -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: make

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

(defun file-content (file &optional delete-header-comment)
  "Return content of the given FILE.

If DELETE-HEADER-HEADER is non-nil, this function will delete
its documentation header."
  (with-temp-buffer
    (insert-file-contents file)
    (when delete-header-comment
      (let ((st (line-beginning-position)))
        (search-forward ";;; Code:")
        (delete-region st (point))))
    (delete-blank-lines)
    (buffer-string)))

(defun make-autoload-file ()
  "Fetch `embla-autoload-directory' to generate autoload file.
If the compile directory is not created, we ensure it."
  (unless (file-exists-p embla-compile-directory)
    (make-directory embla-compile-directory))
  (let ((generated-autoload-file embla-autoload-file))
    (dolist (file (directory-files-recursively embla-autoload-directory ""))
      (update-file-autoloads file t generated-autoload-file))))

(defun make-config-file ()
  "Merge files located in `embla-lisp-directory' into one file.

Files in the root and into autoload directory is skipped because
they are already been loaded."
  (let ((content))
    (fetch-subdirectories embla-lisp-directory
      (unless (string-equal name "autoload")
        (dolist (file (directory-files-recursively directory ""))
          (push (file-content file t) content))))
    (write-region
      (mapconcat #'identity content "\n") nil embla-config-file)))


(provide 'make)

;;; make.el ends here
