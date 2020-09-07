;;; lisp/core-autoloads.el --- autoloads generator -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: autoloads

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

(defun embla-auto-install-packages ()
  "Fetchs `embla-modules' directories and loads up files."
  (fetch-embla-files embla-modules
    (unless (string-match "\\load-up.el\\'" file)
      (load file nil 'nomessage))))

(defmacro fetch-embla-files (dirs &rest body)
  "Fetchs files in the given DIRS and execute BODY on it.
This function only fetch emacs lisp files."
  `(dolist (dir ,dirs)
     (dolist (file (directory-files-recursively dir ""))
       (when (string-match "\\.el\\'" file)
         ,@body))))

(defun embla-update-autoloads ()
  "Updates autoloads for files defines in lisp directories.
This function fetch `embla-modules' directories."
  (let ((generated-autoload-file embla-autoloads-file))
    (fetch-embla-files embla-modules
      (update-file-autoloads file t generated-autoload-file)))
  (push embla-temporary-directory load-path)
  (require 'embla)
  (embla-bootstrap)
  (embla-auto-install-packages))


(provide 'core-autoloads)

;;; core-autoloads.el ends here
