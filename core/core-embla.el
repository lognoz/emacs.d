;;; core-embla.el - Core Initialization File

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

(defconst embla-version "0.1.1"
  "Current version of Embla.")

(defconst operating-system
  (cond ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'darwin) "mac")
        ((eq system-type 'windows-nt) "windows")
        ((eq system-type 'berkeley-unix) "bsd")))

(defconst current-user
  (getenv (if (eq operating-system "windows") "USERNAME" "USER")))

(defconst embla-core-directory (concat user-emacs-directory "core/")
  "The directory of core files.")

(defconst embla-component-directory (concat user-emacs-directory "component/")
  "The directory of component files.")

(defconst embla-temporary-directory (concat user-emacs-directory "temporary/")
  "The directory of temporary files.")

;; Disable useless GUI components.
(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

;; Use UTF-8 as the default coding system.
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Remove splash and startup screen.
(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Fix Emacs confusion on Windows with HOME and APPDATA,
;; causing `abbreviate-home-dir' to produce incorrect paths.
(when (eq operating-system "windows")
  (setq abbreviated-home-dir "\\`'"))

(defun load-q (path)
  "Load path without message output."
  (unless (file-exists-p path)
    (write-region "" nil path))
  (load path nil 'nomessage))

(defun embla--get-modules (base)
  "Get all directories by a path."
  (let ((modules (list)))
    (dolist (f (directory-files base))
      (when (and (file-directory-p (concat base f))
                 (not (equal f "."))
                 (not (equal f "..")))
        (add-to-list 'modules f)))
    modules))

(defun embla--startup-hook ()
  "Add post init processing."
  (dolist (module (embla--get-modules embla-component-directory))
    (message module)))

(defun embla-initialize ()
  "Bootstrap Embla, if it hasn't already been loaded."
  ;; Place the variables created by Emacs in custom file.
  (load-q (concat user-emacs-directory "custom.el"))
  ;; Load all core configurations and functions dynamically.
  (dolist (f (directory-files embla-core-directory))
    (let ((path (concat embla-core-directory f)))
      (when (and (not (file-directory-p (concat embla-core-directory f)))
                 (not (equal f "core-embla.el")))
        (load-q path))))
  ;; Load pre-init emacs dependencies.
  (embla-setup-elpa-repository)
  (require-package (atom-one-dark-theme))
  (load-theme 'atom-one-dark t)
  ;; Add post init processing to load embla components after emacs startup.
  (add-hook 'emacs-startup-hook 'embla--startup-hook))
