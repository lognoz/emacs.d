;;; lisp/embla.el --- core initialization -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: core
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))

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

(require 'cl-lib)
(require 'gnutls)
(require 'core-vars)
(require 'core-functions)
(require 'core-component)

(defgroup embla nil
  "Embla customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/lognoz/embla"))

(defconst embla-version "0.1.1"
  "The current version of Embla.")

(define-minor-mode embla-mode
  "Minor mode to consolidate Embla extensions."
  :global t
  :group 'embla
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-1") 'delete-other-windows)
    (define-key map (kbd "s-2") 'split-window-below)
    (define-key map (kbd "s-3") 'split-window-right)
    (define-key map (kbd "s-k") 'kill-current-buffer)
    (define-key map (kbd "s-o") 'other-window)
    (define-key map (kbd "s-d") 'dired-jump)
    map))

(defcustom embla-package-archives
  (let ((protocol (if gnutls-verify-error "https" "http")))
    (list (cons "melpa"  (concat protocol "://melpa.org/packages/"))
          (cons "stable" (concat protocol "://stable.melpa.org/packages/"))
          (cons "org"    (concat protocol "://orgmode.org/elpa/"))
          (cons "gnu"    (concat protocol "://elpa.gnu.org/packages/"))))
  "The alist of package archives used as repository."
  :group 'embla
  :type 'cons)

(defvar embla-init-p nil
  "Non-nil if Embla has been initialized.")

(defvar embla-developer-mode-p nil
  "Non-nil if Embla is in developer mode.")

(defvar embla-package-initialize-p nil
  "Non-nil if package has been initialized.")

(defvar embla-package-refresh-contents-p nil
  "Non-nil if package contents has been refreshed.")

(defvar embla-file-name-handler-alist file-name-handler-alist
  "The last `file-name-handler-alist' used to restore its value after startup.")

(defconst embla-private-init-file (expand-file-name "init.el" embla-private-directory)
  "The private initialization file.")

(defconst embla-font-lock-keywords
  (eval-when-compile
    `((,(regexp-font-lock '("define-component"))
        (1 font-lock-keyword-face)
        (2 font-lock-function-name-face nil t))
      (,(regexp-font-lock '("require-package"))
        (1 font-lock-keyword-face)
        (2 font-lock-constant-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode embla-font-lock-keywords)

(defun embla-display-version ()
  "Display the current `embla-version' in the minibuffer."
  (interactive)
  (message "embla (version %s)" embla-version))

(defun embla-bootstrap ()
  "Bootstrap Embla, if it hasn't already."
  (unless (file-exists-p embla-temporary-directory)
    (make-directory embla-temporary-directory))
  (push embla-temporary-directory load-path)
  (require-lisp-autoloads)
  (require-site-lisp-autoloads)
  (embla-mode t)
  (embla-optimize-startup)
  (embla-set-coding-system)
  (embla-set-custom-file)
  (embla-load-private-init)
  (package-bootstrap)
  (setq embla-init-p t))

(defun embla-set-coding-system ()
  "Use UTF-8 as the default coding system."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))

(defun embla-load-private-init ()
  "Load the init.el located in `embla-private-directory'."
  (when (file-exists-p embla-private-init-file)
    (load embla-private-init-file nil 'nomessage)))

(defun embla-set-custom-file ()
  "Load the variables created by Emacs located in custom file."
  (setq custom-file (expand-file-name "custom.el" embla-temporary-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

(defun embla-optimize-startup ()
  "Change some defaults settings for better launch time."
  (disable-initialize-package)
  (fix-garbage-collection-for-startup)
  (setq enable-dir-local-variables nil))

(defun require-lisp-autoloads ()
  "Require autoloads located in `lisp' directory."
  (when (or (not (file-exists-p embla-lisp-autoloads-file))
            embla-developer-mode-p)
    (require 'autoloads)
    (refresh-lisp-autoloads))
  (require 'embla-lisp-autoloads))

(defun require-site-lisp-autoloads ()
  "Require autoloads located in `site-lisp' directory."
  (when (or (not (file-exists-p embla-site-lisp-autoloads-file))
            embla-developer-mode-p)
    (require 'autoloads)
    (refresh-site-lisp-autoloads))
  (require 'embla-site-lisp-autoloads))

(defun disable-initialize-package ()
  "Disable auto-initialize package."
  (setq package-enable-at-startup nil)
  (setq package--init-file-ensured t))

(defun fix-garbage-collection-for-startup ()
  "Change the frequency of garbage collection."
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 0.6)
  (setq file-name-handler-alist nil))

(defun start-emacs-server ()
  "Start emacs server if it has not already been started."
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(provide 'embla)

;;; embla.el ends here
