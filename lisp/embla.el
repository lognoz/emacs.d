;;; lisp/embla.el --- core initialization -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: core

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

(eval-when-compile
  (require 'cl-lib))

(require 'core-vars)
(require 'core-macros)

(defgroup embla nil
  "Embla customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/lognoz/embla"))

(define-minor-mode embla-mode
  "Minor mode to consolidate Embla extensions."
  :global t
  :group 'embla
  :keymap (make-sparse-keymap))

(defconst embla-private-init-file (expand-file-name "init.el" embla-private-directory)
  "The private initialization file.")

(defvar embla-init-p nil
  "Non-nil if Embla has been initialized.")

(defvar embla-file-name-handler-alist file-name-handler-alist
  "Last `file-name-handler-alist' used to restore its value after startup.")

(defun embla-bootstrap ()
  "Bootstrap Embla, if it hasn't already."
  (unless embla-init-p
    (require 'embla-autoloads)
    (embla-optimize-startup)
    (embla-set-coding-system)
    (embla-initialize-custom-file)
    (embla-initialize-private-init)
    (package-bootstrap)
    (embla-mode t)
    (setq embla-init-p t)))

(defun embla-set-coding-system ()
  "Use UTF-8 as the default coding system."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))

(defun embla-initialize-private-init ()
  "Loads the init.el located in `embla-private-directory'."
  (unless (file-exists-p embla-private-init-file)
    (load embla-private-init-file nil 'nomessage)))

(defun embla-initialize-custom-file ()
  "Loads the variables created by Emacs located in custom file."
  (setq custom-file (expand-file-name "custom.el" embla-temporary-directory))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

(defun embla-optimize-startup ()
  "Changes somes defaults settings for better launch time."
  ;; Disable auto-initialize package.
  (setq package-enable-at-startup nil
        package--init-file-ensured t)
  ;; Change the frequency of garbage collection.
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        file-name-handler-alist nil)
  ;; Disable local variable before to create autoload files.
  (setq enable-dir-local-variables nil))

(defun restore-values ()
  "Restores default values after startup."
  (setq file-name-handler-alist embla-file-name-handler-alist
        gc-cons-threshold 16000000
        gc-cons-percentage 0.1))


(provide 'embla)

;;; embla.el ends here
