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

(require 'cl-lib)

(defconst operating-system
  (cond ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'darwin) "mac")
        ((eq system-type 'windows-nt) "windows")
        ((eq system-type 'berkeley-unix) "bsd")))

(defconst is-xorg
  (eq window-system 'x))

(defconst current-user
  (getenv (if (eq operating-system "windows") "USERNAME" "USER")))

(defconst embla-core-directory (concat user-emacs-directory "core/")
  "The directory of core files.")

(defconst embla-component-directory (concat user-emacs-directory "component/")
  "The directory of component files.")

(defconst embla-language-directory (concat user-emacs-directory "language/")
  "The directory of language files.")

(defconst embla-temporary-directory (concat user-emacs-directory "temporary/")
  "The directory of temporary files.")

(defvar embla-startup-hook nil
  "Hook called before Emacs is started.")

(defvar embla-after-component-installation nil
  "Hook called after component packages has been install.")

;; Place the variables created by Emacs in custom file.
(setq custom-file (concat embla-temporary-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil 'nomessage)

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

;;; External macro functions.

(defmacro fetch-content (source &rest body)
  `(dolist (f (directory-files ,source))
     (let ((path (concat ,source f))
           (module (file-name-sans-extension f)))
       ,@body)))

(defmacro fetch-dependencies (module &rest body)
  `(when embla-component-packages
    (dolist (dependency embla-component-packages)
      (when-function-exists (concat ,module "/init-" dependency)
        ,@body))))

(defmacro when-function-exists (name &rest body)
  `(let ((func (intern ,name)))
     (when (fboundp func)
       ,@body)))

;;; Internal core functions.

(defun embla//load-composant-files (path)
  (setq embla-component-packages nil)
  (dolist (f '("/config.el" "/packages.el"))
    (when (file-exists-p (concat path f))
      (load (concat path f) nil 'nomessage))))

(defun embla//after-emacs-startup-hook ()
  (fetch-content embla-component-directory
    (when (and (file-directory-p path)
               (not (equal f "."))
               (not (equal f "..")))
      ;; Load composant files in component directory.
      (embla//load-composant-files path)
      ;; Add hook to set all configurations after dependencies installation.
      (fetch-dependencies module
        (add-hook 'embla-after-component-installation func))))

  ;; Add language to auto mode to load what inside language directory
  ;; dynamically.
  (mapc (lambda (entry)
   (let* ((language (car entry))
          (extension (cadr entry))
          (mode (cadr (cdr entry)))
          (path (concat embla-language-directory language)))
      (add-to-list 'auto-mode-alist
       `(,extension . (lambda ()
           (embla//load-composant-files ,path)
           (fetch-dependencies ,language (funcall func))
           (,mode))))))
    embla-languages-alist)

  ;; Execute hook to apply component configurations.
  (run-hooks 'embla-after-component-installation))

;;; External core functions.

(defun embla/initialize ()
  ;; Use `embla-core-directory` path reference to fetch elisp files and load
  ;; them dynamicly. If a startup hook is defined in the file, it will add it
  ;; to embla to execute it after by it's own after this statement.
  (fetch-content embla-core-directory
    (when (and (not (file-directory-p path))
               (not (equal f "core-embla.el")))
      (load path nil 'nomessage)
      (when-function-exists (concat module "/embla-startup-hook")
        (add-hook 'embla-startup-hook func))))
  (run-hooks 'embla-startup-hook)
  (add-hook 'emacs-startup-hook 'embla//after-emacs-startup-hook))
