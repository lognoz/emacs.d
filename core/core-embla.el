;;; core-embla.el --- Core Initialization File

;; Copyright (c) Marc-Antoine Loignon

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

;;; Define Embla minor mode.

(define-minor-mode embla-mode
  "Minor mode to consolidate Emacs Embla extensions."
  :global t
  :keymap (make-sparse-keymap))

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

(defconst embla-template-directory (concat embla-core-directory "template/")
  "The directory of template files.")

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

(defconst embla-build-directory (concat embla-temporary-directory "build/")
  "The directory of temporary files.")

(defconst embla-private-init-file (concat embla-private-directory "init.el")
  "The private initialization file.")

(defconst embla-autoload-file (concat embla-build-directory "embla-autoload.el")
  "The Embla autoload file.")

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

(defun directory-name (path)
  "Return the directory name of a reference path."
  (file-name-nondirectory
   (directory-file-name
     (file-name-directory path))))

(defun directories-list (path)
  "Return a list of directories by a reference path."
  (let ((list))
    (dolist (f (directory-files path))
      (let ((path (concat path f)))
        (when (and (file-directory-p path)
                    (not (equal f "."))
                    (not (equal f "..")))
          (push (file-name-as-directory path) list))))
    list))

(defun template-content (path)
  "Return template content by reference path."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun create-custom-file ()
  "Place the variables created by Emacs in custom file."
  (setq custom-file (concat embla-temporary-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file nil 'nomessage))

(defun import-core (&rest references)
  "This function is used to load core file."
  (dolist (reference references)
    (load (concat embla-core-directory reference)
          nil 'nomessage)))

(defun require-composites ()
  "This function is used to require all Embla composites and to check
if requirements is ensure."
  (import-core "core-editor" "core-package")
  ;; If Embla not installed, use execute installer.
  (when (not (file-exists-p embla-autoload-file))
    (import-core "functionality/functionality-editing"
                 "installer/installer")
    (embla-install-program))
  ;; Load Embla theme
  (load-theme 'atom-one-dark t))

(defun embla-after-init-hook ()
  "This function is used to load packages and config files into
component directory."
  (import-core "core-mode-line")
  ;; Enable minor mode.
  (embla-mode)
  ;; Initialize mode line.
  (mode-line-initialize)
  ;; Require autoload and startup files.
  (add-to-list 'load-path embla-build-directory)
  (require 'embla-autoload)
  ;; Call private initialization file.
  (when (file-exists-p embla-private-init-file)
    (load embla-private-init-file nil 'nomessage))
  ;; Enable local variable to load .dir-locals.el.
  (setq enable-dir-local-variables t)
  ;; Define bookmark, minibuffer, history, place, undo-tree
  ;; and backup files.
  (add-hook 'pre-command-hook 'define-context-files)
  ;; Clear initialization component message.
  (message ""))

(defun define-context-files ()
  "This function is used to set bookmark, minibuffer, history, place,
undo-tree and backup files."
  ;; Import file function.
  (import-core "core-file")
  ;; Define bookmark file.
  (setq bookmark-default-file (concat embla-temporary-directory "bookmark"))
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
  (setq save-place-file (concat embla-temporary-directory "saveplace"))
  ;; Set undo tree.
  (file-set-undo-tree)
  ;; Set backup files.
  (file-set-backup))

;;; External core functions.

(defun embla-initialize ()
  "This function is the main initialization function. It create custom
and autoload file to manage Emacs configuration."
  (setq inhibit-default-init t
        inhibit-splash-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  ;; Place the variables created by Emacs in custom file.
  (create-custom-file)
  ;; Require composites.
  (require-composites)
  ;; Remove mode line for loading.
  (setq-default mode-line-format nil)
  ;; Add hook after Emacs init.
  (add-hook 'after-init-hook 'embla-after-init-hook))

(provide 'core-embla)
