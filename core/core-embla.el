;;; core/core-embla.el --- core initialization file -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: core init

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

;;; Environmental constants.

(defconst operating-system
  (cond ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'darwin) "mac")
        ((eq system-type 'windows-nt) "windows")
        ((eq system-type 'berkeley-unix) "bsd")))

(defconst is-xorg
  (eq window-system 'x))

(defconst is-archlinux
  (string-match-p "ARCH" operating-system-release))

(defconst current-user
  (getenv (if (eq operating-system "windows") "USERNAME" "USER")))

;;; Contextual Embla constants.

(defconst embla-core-init (concat user-emacs-directory "init.el")
  "The Embla file reference.")

(defconst embla-core-directory (concat user-emacs-directory "core/")
  "The directory of core files.")

(defconst embla-component-directory (concat user-emacs-directory "component/")
  "The directory of component files.")

(defconst embla-private-directory (concat user-emacs-directory "private/")
  "The directory of private files.")

(defconst embla-snippet-directory (concat embla-private-directory "snippet/")
  "The directory of snippets files.")

(defconst embla-temporary-directory (concat embla-private-directory "temporary/")
  "The directory of temporary files.")

(defconst embla-build-directory (concat embla-temporary-directory "build/")
  "The directory of temporary files.")

(defconst embla-private-init-file (concat embla-private-directory "init.el")
  "The private initialization file.")

(defconst embla-autoload-file (concat embla-build-directory "embla-autoload.el")
  "The Embla autoload file.")

;;; Define Embla minor mode.

(define-minor-mode embla-mode
  "Minor mode to consolidate Emacs Embla extensions."
  :global t
  :keymap (make-sparse-keymap))

;;; Emacs core configuration.

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;; External macro functions.

(defmacro fetch-content (path &rest body)
  "Macro used to fetch content and easily execute action on
target file."
  `(dolist (path (directories-list ,path))
     (let ((module (directory-name path)))
       ,@body)))

(defmacro when-function-exists (name &rest body)
  "Macro used to convert a string to a function reference and
that can quickly execute action with it."
  `(let ((func (intern ,name)))
     (when (fboundp func)
       ,@body)))

;;; Internal core functions.

(defun create-custom-file ()
  "Place the variables created by Emacs in custom file."
  (setq custom-file (concat embla-temporary-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

;;; External core functions.

(defun embla-initialize ()
  "Load files to manage Emacs configuration."
  ;; Place the variables created by Emacs in custom file.
  (create-custom-file))

(provide 'core-embla)
