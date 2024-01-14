;;; lisp/load-up.el --- Load up -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon <developer@lognoz.org>

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/embla
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

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

(require 'gnutls)


;;; --- Constant contexts

(defconst embla-site-lisp-directory (expand-file-name "site-lisp/" user-emacs-directory)
  "The directory of site lisp files.")

(defconst embla-bin-directory (expand-file-name "bin/" user-emacs-directory)
  "The directory of bin files.")

(defconst embla-temporary-directory (expand-file-name "temporary/" user-emacs-directory)
  "The directory of temporary files.")

(defconst embla-private-directory (expand-file-name "private/" user-emacs-directory)
  "The directory of private files.")

(defconst embla-lisp-autoloads-file (expand-file-name "embla-lisp-autoloads.el" embla-temporary-directory)
  "The main autoloads file.")

(defconst embla-site-lisp-autoloads-file (expand-file-name "embla-site-lisp-autoloads.el" embla-temporary-directory)
  "The autoloads file for `site-lisp' directory.")


;;; --- Embla mode

(defgroup embla nil
  "Embla customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/lognoz/embla"))

(defconst embla-version "0.1.0"
  "The current version of Embla.")

(define-minor-mode embla-mode
  "Minor mode to consolidate Embla extensions."
  :init-value t
  :global t
  :group 'embla
  :keymap (make-sparse-keymap))


;;; --- Command line

(defvar embla-command-line-p nil
  "Non-nil if it's the command line environment.")

(defmacro embla-eval-on-install (&rest body)
  "Require elpa PACKAGE while executing BODY."
  `(when embla-command-line-p
     ,@body))

(defun embla-install-or-reinstall ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (async-shell-command
     (concat embla-bin-directory "embla install"))))


;;; --- Packages extension

(defvar embla-package-initialize-p nil
  "Non-nil if package has been initialized.")

(defcustom embla-package-archives
  (let ((protocol (if gnutls-verify-error "https" "http")))
    (list (cons "melpa"  (concat protocol "://melpa.org/packages/"))
          (cons "org"    (concat protocol "://orgmode.org/elpa/"))
          (cons "gnu"    (concat protocol "://elpa.gnu.org/packages/"))))
  "The alist of package archives used as repository."
  :group 'embla
  :type 'cons)

(defun embla-package-bootstrap ()
  "Refresh package contents define in `embla-package-archives'."
  (unless embla-package-initialize-p
    (setq package-archives embla-package-archives)
    (setq package-enable-at-startup nil)
    (setq embla-package-initialize-p t)
    (package-initialize)
    (let ((inhibit-message nil))
      (package-refresh-contents))))

(defmacro embla-builtin-package (package &rest body)
  "Require builtin PACKAGE while executing BODY."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'embla-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro embla-elpa-package (package &rest body)
  "Require elpa PACKAGE while executing BODY."
  (declare (indent 1))
  `(progn
     (when embla-command-line-p
       (embla-package-bootstrap)
       (let ((inhibit-message nil))
         (message (format "Install %s..." ,package)))
       (when (not (package-installed-p ,package))
         (package-install ,package)))
         ;;(unless
           ;; (ignore-errors
           ;; (package-reinstall ,package))
           ;; (display-warning 'embla-emacs (format "Package `%s' failed to reinstall" ,package) :warning)
     (if (require ,package nil 'noerror)
         (unless embla-command-line-p
           (progn ,@body))
       (display-warning 'embla-emacs (format "Loading `%s' failed" ,package) :warning))))

(defmacro embla-site-lisp-package (remote-path &rest body)
  "Retrieve REMOTE-PATH in site lisp directory while executing BODY."
  (declare (indent 1))
  `(progn
     (let* ((package-name (string-trim-right (file-name-nondirectory ,remote-path) ".el"))
            (dest (expand-file-name package-name embla-site-lisp-directory)))
       (unless (file-directory-p dest)
         (let ((default-directory embla-site-lisp-directory))
           (shell-command (format "git clone %s %s" ,remote-path package-name)))))
     (unless embla-command-line-p
       ,@body)))


;;; --- Modules autoload

(defmacro embla-autoload (file &rest events)
  "Autoload lisp component FILE based on their EVENTS."
  (declare (indent 1))
  `(let* ((file (expand-file-name (concat "lisp/" ,file) user-emacs-directory))
          (events ',events)
          (self-loader (lambda (&rest _)
                         (load file nil 'nomessage))))
     (when (null events)
       (setq events '(emacs-startup-hook)))
     (mapcar (lambda (event)
               (if (functionp event)
                   (advice-add event :before self-loader)
                 (add-hook event self-loader)))
             events)))

(defun embla-compile (dir outfile)
  "Update the autoloads for DIR.
Make sure to give an absolute path as OUTFILE."
  (require 'cl-lib)
  (delete-file outfile)
  (let ((dirs))
    (push dir dirs)
    (dolist (file (directory-files-recursively dir "" 't))
      (when (file-directory-p file)
        (push (expand-file-name file dir) dirs)))
    (loaddefs-generate dirs outfile)))

(defun embla-reload ()
  "Reloads your private config and core."
  (interactive)
  (embla-compile (file-name-directory load-file-name) embla-lisp-autoloads-file)
  (require 'embla-lisp-autoloads nil 'noerror))

(defun embla-reload-site-lisp ()
  "Reloads third party lisp."
  (interactive)
  (embla-compile embla-site-lisp-directory embla-site-lisp-autoloads-file)
  (require 'embla-site-lisp-autoloads nil 'noerror))

(push embla-temporary-directory load-path)
(push embla-site-lisp-directory load-path)


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; load-up.el ends here
