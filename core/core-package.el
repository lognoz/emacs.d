;;; core-package.el --- Core Package File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: packages

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

(require 'package)

(defvar embla-package-initialized nil
  "Package content has been initialized.")

(defvar embla-component-packages nil
  "List of packages required by component.")

;;; External core functions.

(defun define-keybinding-args-normalize (args)
  "Return normalized `define-keybinding' arguments by allowed variables."
  (let ((plist-grouped) (plist) (definition) (is-definition))
    (while args
      (let ((arg (car args)))
        (setq is-definition nil)
        (when (member arg '(:normal :visual))
          (when plist
            (add-to-list 'plist-grouped `(,definition ,plist) t))
          (setq plist nil)
          (setq definition arg)
          (setq is-definition t))
        (when (and (not is-definition) definition)
          (setq plist
            (append plist (list arg)))))
      (setq args (cdr args)))
    (when plist
      (add-to-list 'plist-grouped `(,definition ,plist) t))
    plist-grouped))

(defun define-keybinding (&rest args)
  ""
  (let ((mode (plist-get args :mode)))
    (unless mode
      (error "Mode need to be specify in 'bind-key' function."))
    (dolist (arg (define-keybinding-args-normalize args))
      (let ((bindings (car (cdr arg))))
        (apply #'evil-define-key* 'normal (symbol-value mode) bindings)))))

(defun define-word-syntax (word-syntax)
  "Define word syntax entry by list of characters."
  (dolist (character word-syntax)
    (modify-syntax-entry
      (cond ((string-equal character "_") ?_)
            ((string-equal character "-") ?-)
            ((string-equal character "\\") ?\\)
            ((string-equal character "$") ?$)) "w")))

(defun package-set-archives ()
  "Set package archives and initialize it."
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize))

(defun define-word-syntax (word-syntax)
  "Define word syntax entry by list of characters."
  (dolist (character word-syntax)
    (modify-syntax-entry
      (cond ((string-equal character "_") ?_)
            ((string-equal character "-") ?-)
            ((string-equal character "\\") ?\\)
            ((string-equal character "$") ?$)) "w")))

(defun require-package (package &optional built-in)
  "This function is the main Embla functionality used to
pre-install package into installer.el."
  (interactive)
  (when (not built-in)
     ;; Add archive and initialize package.
     (unless embla-package-initialized
       (package-set-archives)
       (setq embla-package-initialized t))
     ;; Install package if not installed.
     (unless (package-installed-p package)
       (unless package-archive-contents
         (package-refresh-contents))
       (package-install package)))
  ;; Push value into `embla-component-packages' variable.
  (add-to-list 'embla-component-packages (format "%s" package)))

(provide 'core-package)
