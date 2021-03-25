;;; lisp/core-functions.el --- core functions -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: core functions

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

(defmacro autoloads (&rest args)
  "Execute multiple autoloads with only one statement."
  `(dolist (arg '(,@args))
    (let* ((reference (car arg))
           (funcs (remove reference arg)))
      (dolist (func funcs)
        (autoload func reference)))))

(defun regexp-font-lock (strings)
  "Return font lock regex with STRINGS."
  (concat "(" (regexp-opt strings t) "\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"))

(defmacro eval-before-init (&rest body)
  "Execute BODY if `autoloads' is require.
This function will do nothing if embla is already initialize."
  `(with-eval-after-load 'autoloads
     (unless embla-init-p
       ,@body)))

(defmacro bind-keys (keymap &rest args)
  "Bind multiple keys on KEYMAP."
  `(cl-loop for (key-name . command) in '(,@args)
            do (define-key ,keymap (kbd key-name) command)))

(defun define-syntax-entries (&rest word-syntax)
  "Define word syntax entries by list of characters."
  (dolist (character word-syntax)
    (modify-syntax-entry
      (cond ((string-equal character "_") ?_)
            ((string-equal character "-") ?-)
            ((string-equal character "\\") ?\\)
            ((string-equal character "$") ?$)) "w")))

(defun clear-keys (keymap &rest args)
  "Unbind multiple keys on KEYMAP."
  (dolist (key-name args)
    (define-key keymap (kbd key-name) nil)))

(defun bind-patterns (mode patterns)
  "Add PATTERNS to corresponding major MODE functions."
  (dolist (regex patterns)
    (add-to-list 'auto-mode-alist (cons regex mode))))

(defun require-package (package)
  "Ensure PACKAGE if it's not installed."
  (unless embla-package-initialize-p
    (package-bootstrap))
  (unless (package-installed-p package)
    (package-archives-bootstrap)
    (package-install package)))

(defun directory-in-site-p (directory-name)
  "Non-nil if the DIRECTORY-NAME exists in `embla-site-lisp-directory'."
  (file-directory-p (expand-file-name directory-name embla-site-lisp-directory)))

(defun clone-repository (path &optional dest)
  "Process git clone command on DEST directory with PATH."
  (require-program "git")
  (unless dest
    (setq dest embla-site-lisp-directory))
  (let ((default-directory dest))
    (shell-command (format "git clone %s" path))))

(defun require-program (program)
  "Check for system PROGRAM and printing error if not found."
  (unless (executable-find program)
    (user-error "Required program \"%s\" not found in your path" program)))

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


(provide 'core-functions)

;;; core-functions.el ends here
