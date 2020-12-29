;;; lisp/load-up.el --- load up embla files -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: embla

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

(defconst embla-lisp-directory (file-name-directory load-file-name)
  "The directory of lisp files.")

(defconst embla-site-lisp-directory (expand-file-name "site-lisp/" user-emacs-directory)
  "The directory of site-lisp packages.")

(defconst embla-private-directory (expand-file-name "private/" user-emacs-directory)
  "The directory of private files.")

(defconst embla-temporary-directory (expand-file-name "temporary/" user-emacs-directory)
  "The directory of temporary files.")

(defconst embla-lisp-autoloads-file (expand-file-name "embla-lisp-autoloads.el" embla-temporary-directory)
  "The main autoloads file.")

(defconst embla-site-lisp-autoloads-file (expand-file-name "embla-site-lisp-autoloads.el" embla-temporary-directory)
  "The autoloads file for `site-lisp' directory.")

(defvar embla-modules
  (list (expand-file-name "." embla-lisp-directory)
        (expand-file-name "autoloads" embla-lisp-directory)
        (expand-file-name "editor" embla-lisp-directory)
        (expand-file-name "emacs" embla-lisp-directory)
        (expand-file-name "languages" embla-lisp-directory)
        (expand-file-name "tools" embla-lisp-directory))
  "The list of modules directories.")

;; Add sub-directories to the load-path for files that might get
;; autoloaded when bootstrapping.
(setq load-path (append load-path embla-modules))

;; Bootstrap Embla configurations.
(require 'embla)
(embla-bootstrap)


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; load-up.el ends here
