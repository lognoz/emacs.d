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

(require 'cl-lib)
(require 'const)
(require 'macro)

;;; --- Minor mode

(define-minor-mode embla-mode
  "Minor mode to consolidate Embla extensions."
  :global t
  :keymap (make-sparse-keymap))

(defgroup embla nil
  "Embla customizations."
  :group 'embla-mode
  :link '(url-link :tag "Homepage" "https://github.com/lognoz/embla"))


;;; --- Global variables

(defvar embla-init-p nil
  "Non-nil if Embla has been initialized.")

(defvar embla-test-environment-p nil
  "Non-nil if it's started in test environment.")


;;; --- Emacs configuration

;; Remove splash screen and reduce noice at startup.
(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Use UTF-8 as the default coding system.
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Remove mode line on loading.
(setq-default mode-line-format nil)

;; Disabling UI elements.
(unless emacs-version-above-27-p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))


;;; --- Internal functions

(defun create-custom-file ()
  "Place the variables created by Emacs in custom file."
  (setq custom-file (concat embla-temporary-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

(defun boot-components ()
  (catch-component embla-autoload-directory embla-autoload-file
    (make-autoload-file))
  (add-to-list 'load-path embla-compile-directory)
  (require 'embla-autoload)
  (catch-component embla-lisp-directory embla-config-file
    (make-config-file))
  (load embla-config-file nil 'nomessage)
  (boot-package))

(defmacro catch-component (source destination &rest body)
  `(when (file-newer-than-file-p ,source ,destination)
    (require 'make)
    ,@body))

(defun load-private-init ()
  "Include content loacted in user init."
  (load embla-private-init-file nil 'nomessage))

(defun restore-values ()
  "Restore default values after startup."
  (setq file-name-handler-alist last-file-name-handler-alist
        gc-cons-threshold 16000000
        gc-cons-percentage 0.1))


(provide 'embla)

;;; embla.el ends here
