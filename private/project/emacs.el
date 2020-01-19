 ;;; project.el --- Emacs Project File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: emacs project

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

(defvar emacs--last-selection nil)

(defconst emacs--source-directory (concat embla-project-directory "template/")
  "The directory of template files.")

(defvar emacs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\p\c" 'emacs-create)
    (define-key keymap "\C-c\p\l" 'emacs-reload)
    keymap))

;;; Internal project functions.

(defun emacs--replace-variables (file replacements)
  (with-temp-file file
    (insert-file-contents-literally file)
    (mapc (lambda (entry)
      (setq entry (eval entry))
      (goto-char 0)
      (while (search-forward (car entry) nil t)
        (replace-match (cdr entry))))
      replacements)))

(defun emacs--form-core (name keyword)
  (interactive "sCore name: \nsKeywords: ")
  (emacs--create-action
    "core" name keyword))

(defun emacs--form-component (name keyword)
  (interactive "sComponent name: \nsKeywords: ")
  (emacs--create-action
    "component" name keyword))

(defun emacs--create-action (type name keyword)
  ;; Trim anwser value and check if `name' and `keyword'
  ;; variable is empty.
  (dolist (element '(name keyword))
    (let ((value (string-trim (symbol-value element))))
      ;; To update a symbol value, you need to use set instead of setq.
      ;; Reference: https://bit.ly/2RpmVpb
      (set element value)
      (when (string-equal value "")
        (error (concat "Error: " (symbol-name element) " can't be empty.")))))

  (let* ((slugify-name (replace-regexp-in-string " " "-" (downcase name)))
         (slug slugify-name)
         (capitalize-name (capitalize name))
         (path-destination nil)
         (path-validation nil)
         (references nil)
         (content ""))

    (cond
      ;; Define configuration for core.
      ((string-equal type "core")
        (setq slug (concat "core-" slug ".el")
              content (get-file-content (expand-file-name "core-hook.el" emacs--source-directory))
              path-validation embla-core-directory
              path-destination embla-core-directory)
        (add-to-list 'references slug))

      ;; Define configuration for component.
      ((string-equal type "component")
        (setq path-validation embla-component-directory
              path-destination (concat embla-component-directory slug)
              references '("config.el" "packages.el"))))

    ;; Check if there is already a composite with that name.
    (dolist (f (directory-files path-validation))
      (when (string-equal slug f)
        (error (concat "Error: There is already a " type " directory with that name."))))

    ;; Create component directory.
    (when (string-equal type "component")
      (make-directory path-destination))

    ;; Copy and replace varaibles.
    (dolist (f references)
      (let ((path (expand-file-name f path-destination)))
        (copy-file
          (expand-file-name "header.el" emacs--source-directory) path)

        ;; Inject content in file.
        (emacs--replace-variables path
          '((cons "__content__" content)))

        ;; Inject variables in file.
        (emacs--replace-variables path
          '((cons "__title__"
              (concat capitalize-name
                      (cond ((string-equal f "config.el") " Component")
                            ((string-equal f "packages.el") " Packages Component")
                            (t " Initialization"))))
            (cons "__file__" f)
            (cons "__hook__" slugify-name)
            (cons "__name__" capitalize-name)
            (cons "__keyword__" (downcase keyword))
            (cons "__year__" (format-time-string "%Y"))))

        (when (string-equal f (car (last references)))
          (find-file path)
          (end-of-buffer))))))

;;; External project functions.

(defun emacs-create ()
  "Create files into core and component directories and add package rapidly."
  (interactive)
  (let ((target (helm :sources (helm-build-sync-source "Create Template"
                               :candidates '(component core)
                               :fuzzy-match t)
                      :preselect emacs--last-selection)))
    (setq emacs--last-selection target)
    (when-function-exists (concat "emacs--form-" target)
      (setq emacs--last-selection target)
      (call-interactively func))))

(defun emacs-reload ()
  "Reload init configuration."
  (interactive)
  (load-file embla-core-init))

;;; Define minor mode.

(define-minor-mode emacs-mode
  "A minor-mode to help to manage Emacs configuration."
  nil " emacs" emacs-mode-map)
