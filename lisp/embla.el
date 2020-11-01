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
(require 'core-vars)

(defgroup embla nil
  "Embla customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/lognoz/embla"))

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

(defvar embla-init-p nil
  "Non-nil if Embla has been initialized.")

(defvar embla-packages nil
  "The list of packages Embla needs to install.")

(defvar embla-first-file-hook nil
  "The hooks run before the first interactively opened file.")

(defvar embla-file-name-handler-alist file-name-handler-alist
  "The Last `file-name-handler-alist' used to restore its value after startup.")

(defconst embla-private-init-file (expand-file-name "init.el" embla-private-directory)
  "The private initialization file.")

(defun embla-bootstrap ()
  "Bootstrap Embla, if it hasn't already."
  (unless embla-init-p
    (load-package-file)
    (require 'embla-lisp-autoloads)
    (setq embla-init-p t)
    (embla-optimize-startup)
    (embla-set-coding-system)
    (embla-set-custom-file)
    (embla-load-private-init)
    (embla-load-site-lisp-autoloads)
    (embla-set-hooks)
    (package-bootstrap)
    (embla-mode t)))

(defun embla-set-coding-system ()
  "Use UTF-8 as the default coding system."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))

(defun embla-load-site-lisp-autoloads ()
  "Load autoloads located in `site-lisp' directory."
  (unless (file-exists-p embla-site-lisp-autoloads-file)
    (refresh-site-lisp-autoloads))
  (require 'embla-site-lisp-autoloads))

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
  "Change somes defaults settings for better launch time."
  ;; Disable auto-initialize package.
  (setq package-enable-at-startup nil
        package--init-file-ensured t)
  ;; Change the frequency of garbage collection.
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        file-name-handler-alist nil)
  ;; Disable local variable before to create autoload files.
  (setq enable-dir-local-variables nil))

(defun load-package-file ()
  "Load package file located in `embla-lisp-directory'."
  (load (expand-file-name "package" embla-lisp-directory)
        nil 'nomessage))

(defun restore-values ()
  "Restore default values after startup."
  (setq file-name-handler-alist embla-file-name-handler-alist
        gc-cons-threshold 16000000
        gc-cons-percentage 0.1))

(defun start-emacs-server ()
  "Start emacs server if it has not already been started."
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(defun embla-set-hooks ()
  "Attach `embla-first-file-hook' to functions."
  (let ((fn `(lambda (&rest _)
               (run-hooks 'embla-first-file-hook))))
    (dolist (on (list 'after-find-file
                      'dired-initial-position-hook))
      (if (functionp on)
          (advice-add on :before fn)
        (add-hook on fn)))))

(defmacro advice (event &rest functions)
  "Attach FUNCTIONS to a hooks or another function.
If the EVENT is not a variable hook it will execute an `advice-add'."
  (let ((fn `(lambda (&rest _)
               (mapcar #'funcall '(,@functions)))))
    (if (functionp event)
        `(advice-add ',event :before ,fn)
      `(add-hook ',event ,fn))))

(defmacro bind-keys (keymap &rest args)
  "Bind multiple keys on KEYMAP."
  `(cl-loop for (key-name . command) in '(,@args)
            do (define-key ,keymap (kbd key-name) command)))

(defun clear-keys (keymap &rest args)
  "Unbind multiple keys on KEYMAP."
  (dolist (key-name args)
    (define-key keymap (kbd key-name) nil)))

(defun boot-packages (&rest packages)
  "Ensure PACKAGES if `embla-update-autoloads' is executed."
  (with-eval-after-load 'core-autoloads
    (unless embla-init-p
      (dolist (package packages)
        (require-package package)))))

(defmacro define-syntax-entries (&rest word-syntax)
  "Define word syntax entries by list of characters."
  `(dolist (character '(,@word-syntax))
     (modify-syntax-entry
      (cond ((string-equal character "_") ?_)
            ((string-equal character "-") ?-)
            ((string-equal character "\\") ?\\)
            ((string-equal character "$") ?$)) "w")))

(defun require-program (program)
  "Check for system PROGRAM and printing error if not found."
  (unless (executable-find program)
    (user-error "Required program \"%s\" not found in your path" program)))

(defun clone-repository (path &optional dest)
  "Process git clone command on DEST directory with PATH."
  (unless dest (setq dest embla-site-lisp-directory))
  (require-program "git")
  (let ((default-directory dest))
    (shell-command (format "git clone %s" path))))

(provide 'embla)

;;; embla.el ends here
