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

(defconst emacs--source-directory (concat embla-project-directory "emacs/template/")
  "The directory of template files.")

(defvar emacs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\p\c" 'emacs-create)
    (define-key keymap "\C-c\p\l" 'emacs-reload)
    keymap))

;;; Internal project functions.

(defun emacs--copy-and-replace (src dest replacements)
  (copy-file src dest)
  (with-temp-file dest
    (insert-file-contents-literally dest)
    (mapc (lambda (entry)
      (setq entry (eval entry))
      (goto-char 0)
      (while (search-forward (car entry) nil t)
        (replace-match (cdr entry))))
      replacements)))

(defun emacs--create-component (component-name keyword)
  (interactive "sComponent name: \nsKeywords: ")
  ;; Trim anwser value and check if `component-name' and `keyword'
  ;; variable is empty.
  (dolist (element '(component-name keyword))
    (let ((value (string-trim (symbol-value element))))
      ;; To update a symbol value, you need to use set instead of setq.
      ;; Reference: https://bit.ly/2RpmVpb
      (set element value)
      (when (string-equal value "")
        (error (concat "Error: " (symbol-name element) " can't be empty.")))))

  (let ((component (replace-regexp-in-string " " "-" (downcase component-name)))
        (capitalize-name (capitalize component-name)))
    ;; Check if there is already a component directory with that name.
    (dolist (f (directory-files embla-component-directory))
      (when (string-equal component f)
        (error "Error: There is already a component directory with that name.")))

    ;; Create component directory.
    (make-directory
      (expand-file-name component embla-component-directory))

    ;; Copy and replace varaibles.
    (dolist (file '("config.el" "packages.el"))
      (emacs--copy-and-replace
        (expand-file-name "header.el" emacs--source-directory)
        (expand-file-name file (concat embla-component-directory component))
        '((cons "__title__"
            (cond ((string-equal file "config.el")
                    (concat capitalize-name " Component"))
                    (t (concat capitalize-name " Packages Component"))))
          (cons "__file__" file)
          (cons "__keyword__" (downcase keyword))
          (cons "__year__" (format-time-string "%Y")))))))

;;; External project functions.

(defun emacs-create ()
  "Create files into core and component directories and add package rapidly."
  (interactive)
  (let ((target (helm :sources (helm-build-sync-source "Create Template"
                               :candidates '(component core)
                               :fuzzy-match t)
                      :preselect emacs--last-selection)))
    (when-function-exists (concat "emacs--create-" target)
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
