;;; lisp/packages.el --- package configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: package

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

;;; --- Global variables

(defvar embla-package-initialize-p nil
  "Non-nil if package has been initialize.")

(defvar embla-package-refresh-contents-p nil
  "Non-nil if package contents has been refreshed.")

;;; --- Custom variables

(defcustom embla-package-archives
  (let ((protocol (if gnutls-verify-error "https" "http")))
    (list (cons "melpa" (concat protocol "://melpa.org/packages/"))
          (cons "org"   (concat protocol "://orgmode.org/elpa/"))
          (cons "gnu"   (concat protocol "://elpa.gnu.org/packages/"))))
  "The alist of package archives used as repository."
  :group 'embla
  :type 'cons)


;;; --- External functions

(defun boot-package ()
  "Set ELPA archives and initialize packages."
  (setq package-archives embla-package-archives)
  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil)
    (setq embla-package-initialized t)
    (package-initialize)))

(defun boot-package-archives ()
  "Refresh package contents define in `embla-package-archives'."
  (unless embla-package-refresh-contents-p
    (package-refresh-contents)
    (setq embla-package-refresh-contents-p t)))

(defun require-package (&rest packages)
  "Ensure PACKAGES if it's not installed."
  (unless embla-package-initialize-p
    (boot-package))
  (dolist (package packages)
    (unless (package-installed-p package)
      (boot-package-archives)
      (package-install package))))


(provide 'packages)

;;; packages.el ends here
