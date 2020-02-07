;;; core-embla.el --- Core Initialization File

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
(require 'core-func)
(require 'core-file)
(require 'core-editor)
(require 'core-package)

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

(defconst embla-mode-directory (concat user-emacs-directory "mode/")
  "The directory of mode files.")

(defconst embla-private-directory (concat user-emacs-directory "private/")
  "The directory of private files.")

(defconst embla-snippet-directory (concat embla-private-directory "snippet/")
  "The directory of snippets files.")

(defconst embla-project-directory (concat embla-private-directory "project/")
  "The directory of project files.")

(defconst embla-temporary-directory (concat embla-private-directory "temporary/")
  "The directory of temporary files.")

(defconst embla-private-init-file (concat embla-private-directory "init.el")
  "The private initialization file.")

(defconst embla-autoload-file (concat embla-temporary-directory "embla-autoload.el")
  "The Embla autoload file.")

;;; Hook used by Embla.

(defvar embla-after-component-installation nil
  "Hook called after component packages has been install.")

;;; External macro functions.

(defmacro fetch-content (path &rest body)
  "Macro used to fetch directory content and easily execute action on
target file."
  `(dolist (f (directory-files ,path))
     (let ((path (concat ,path f))
           (module (file-name-sans-extension f)))
       ,@body)))

(defmacro fetch-dependencies (module &rest body)
  "Fetch component dependencies dynamically installed by Embla so it
could be easy to call function to its config file."
  `(when embla-component-packages
    (dolist (dependency embla-component-packages)
      (when-function-exists (concat ,module "/hook-" dependency)
        (funcall func))
      (when-function-exists (concat ,module "/init-" dependency)
        ,@body))))

(defmacro when-function-exists (name &rest body)
  "Convert a string to a function reference and that can quickly
execute action with it."
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

(defun remove-components ()
  "Disable GUI components."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun create-autoload-file (&optional force)
  "This function parse magic comments locate in core and project
directories and append it to autoload file locate. It will helps to
optimize Embla."
  (when (or (not (file-exists-p embla-autoload-file))
            (equal force t))
    (let ((generated-autoload-file embla-autoload-file))
      ;; Clear content in autoload file.
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert "")
        (save-buffer))
      ;; Update autoload in core directory.
      (update-directory-autoloads embla-core-directory)
      ;; Update autoload with recursive directories found in project.
      (dolist (path (recursive-directories embla-project-directory))
        (update-directory-autoloads path))))
  ;; Require autoload file locate.
  (require 'embla-autoload embla-autoload-file))

(defun define-context-files ()
  "This function is used to set bookmark, minibuffer, history, place,
undo-tree and backup files."
  ;; Define bookmark file.
  (setq bookmark-default-file
        (concat embla-temporary-directory "bookmark"))
  ;; Save minibuffer.
  (savehist-mode 1)
  (setq savehist-file (concat embla-temporary-directory "savehist")
        history-length 100)
  ;; Record history.
  (recentf-mode 1)
  (setq recentf-save-file (concat embla-temporary-directory "recentf")
        recentf-max-menu-items 10
        recentf-max-saved-items 100
        recentf-show-file-shortcuts-flag nil)
  ;; Save cursor positions.
  (save-place-mode 1)
  (setq save-place-file
        (concat embla-temporary-directory "saveplace"))
  ;; Set undo tree.
  (file-set-undo-tree)
  ;; Set backup files.
  (file-set-backup))

(defun load-component-files (path)
  "This function is used to load packages and config files into
component directory."
  (setq embla-component-packages nil)
  (dolist (f '("/packages" "/config"))
    (when (file-exists-p (concat path f ".el"))
      (load (concat path f) nil 'nomessage))))

(defun require-mode (entry)
  "Add language to auto mode, load what inside mode directory
dynamically and define word syntax."
  (let* ((mode-name (car entry))
         (extension (cadr entry))
         (word-syntax (cadr (cdr entry)))
         (mode (cadr (cdr (cdr entry))))
         (hook (concat (symbol-name mode) "-hook"))
         (path (concat embla-mode-directory mode-name)))

    (when extension
      ;; Add mode to auto-mode-alist and install package if it's not
      ;; included by default in Emacs.
      (add-to-list 'auto-mode-alist
        `(,extension . (lambda ()
          (when (not (fboundp ',mode))
            (require-package ',mode))
          (,mode)))))

    ;; Load module in mode directory and define word syntax.
    (add-hook (intern hook) `(lambda ()
      (load-component-files ,path)
      (when ,word-syntax
        (dolist (character ,word-syntax)
          (modify-syntax-entry
            (cond ((string-equal character "_") ?_)
                  ((string-equal character "-") ?-)
                  ((string-equal character "\\") ?\\)
                  ((string-equal character "$") ?$)) "w")))))))

(defun embla-after-startup-hook ()
  "This function is used to load packages and config files into
component directory."
  (fetch-content embla-component-directory
    (when (and (file-directory-p path)
               (not (equal f "."))
               (not (equal f "..")))
      ;; Load component files in component directory.
      (load-component-files path)
      ;; Add hook to set all configurations after dependencies installation.
      (fetch-dependencies module
        (add-hook 'embla-after-component-installation func))))
  ;; Require mode into `embla-mode-alist'.
  (mapc 'require-mode embla-mode-alist)
  ;; Execute hook to apply component configurations.
  (run-hooks 'embla-after-component-installation)
  ;; Call private initialization file.
  (when (file-exists-p embla-private-init-file)
    (load embla-private-init-file nil 'nomessage)))

;;; External core functions.

(defun embla-initialize ()
  ;; Remove splash and startup screen.
  (setq inhibit-default-init t
        inhibit-splash-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)

  ;; Remove mode line for loading.
  (setq-default mode-line-format nil)

  ;; Remove GUI components.
  (remove-components)

  ;; Place the variables created by Emacs in custom file.
  (create-custom-file)

  ;; Create autoload for optimization and performance.
  (create-autoload-file)

  ;; Enable local variable to load .dir-locals.el.
  (setq enable-dir-local-variables t)

  ;; Define bookmark, minibuffer, history, place, undo-tree
  ;; and backup files.
  (define-context-files)

  ;; Add hook after Emacs startup.
  (add-hook 'emacs-startup-hook 'embla-after-startup-hook))

(provide 'core-embla)
