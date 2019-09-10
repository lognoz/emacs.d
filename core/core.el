;;; core.el - Core Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: core init

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

(defmacro add-package (package-list)
  "Install packages if not installed."
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun define-mode (mode &rest patterns)
  "Add entries to auto-mode-alist."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun core/init ()
  "Perform startup initialization."
  (core/disable-gui)
  (core/setup-encoding)
  (core/setup-custom-file)
  (core/setup-elpa-repository)
  (core/setup-theme)
  (core/load-component))

(defun core/load-component ()
  "Load files in component directory."
  (require 'component-evil)
  (require 'component-company)
  (require 'component-web)
  (require 'component-version-control))

(defun core/setup-elpa-repository ()
  "Create an ELPA repository."
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun core/setup-theme ()
  "Setup Emacs theme."
  (add-package (atom-one-dark-theme))
  (load-theme 'atom-one-dark t))

(defun core/setup-custom-file ()
  "Place the variables created by Emacs in custom.el."
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file))

(defun core/setup-encoding ()
  "Define charset and UTF-8 encoding."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))

(defun core/disable-gui ()
  "Disable GUI components."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(provide 'core)
