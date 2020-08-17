;;; lisp/init-embla.el --- core initialization -*- lexical-binding: t; -*-

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
(require 'init-const)


;;; --- Global variables

(defvar embla-init-p nil
  "Non-nil if Embla has been initialized.")


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


;;; --- Internal core functions

(defun create-custom-file ()
  "Place the variables created by Emacs in custom file."
  (setq custom-file (concat embla-temporary-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

(defun embla-after-init-hook ()
  "Load packages and config files into component directory."
  (setq file-name-handler-alist default-file-name-handler-alist
        gc-cons-threshold 16000000))

(defun require-composites ()
  "Require Embla composites and if it's not created ensure it."
  (unless (file-exists-p embla-autoload-file)))


;;; --- External core functions

(defun embla-initialize ()
  "Load files to manage Embla configuration."
  (create-custom-file)
  (require 'init-ui))


(provide 'init-embla)

;;; init-embla.el ends here
