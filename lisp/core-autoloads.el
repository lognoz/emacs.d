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

(defun embla-update-autoloads (dirs)
  "Update autoload definitions for Lisp files in the directories DIRS."
  (let* ((generated-autoload-file embla-autoloads-file))
    (delete-file generated-autoload-file)
    (while dirs
      (let* ((dir (car dirs))
             (files (directory-files-recursively dir "")))
        (while files
          (let ((file (car files)))
            (unless (string-match "\\(elc\\|so\\|dll\\)" (file-name-extension file))
              (update-file-autoloads file t generated-autoload-file))
            (setq files (cdr files))))
        (setq dirs (cdr dirs))))))


(provide 'core-autoloads)

;;; core-autoloads.el ends here
