;;; lisp/package.el --- package manager -*- lexical-binding: t; -*-

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

(defvar embla-package-initialize-p nil
  "Non-nil if package has been initialize.")

(defvar embla-package-refresh-contents-p nil
  "Non-nil if package contents has been refreshed.")

(defcustom embla-package-archives
  (let ((protocol (if gnutls-verify-error "https" "http")))
    (list (cons "melpa" (concat protocol "://melpa.org/packages/"))
          (cons "org"   (concat protocol "://orgmode.org/elpa/"))
          (cons "gnu"   (concat protocol "://elpa.gnu.org/packages/"))))
  "The alist of package archives used as repository."
  :group 'embla
  :type 'cons)

;;;###autoload
(defun package-bootstrap ()
  "Set ELPA archives and initialize packages."
  (unless embla-package-initialize-p
    (setq package-archives embla-package-archives)
    (setq package-enable-at-startup nil)
    (setq embla-package-initialize-p t)
    (package-initialize)))

(defun package-archives-bootstrap ()
  "Refresh package contents define in `embla-package-archives'."
  (unless embla-package-refresh-contents-p
    (package-refresh-contents)
    (setq embla-package-refresh-contents-p t)))

;;;###autoload
(defun require-package (&rest packages)
  "Ensure PACKAGES if it's not installed."
  (unless embla-package-initialize-p
    (package-bootstrap))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-archives-bootstrap)
      (package-install package))))


;;; package.el ends here
