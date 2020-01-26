;;; emacs.el - Emacs Project File

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

(defvar emacs--source-directory (concat embla-project-directory "template/")
  "The directory of template files.")

(defvar emacs-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\pc" 'emacs-create)
    (define-key keymap "\C-c\pl" 'emacs-reload)
    (define-key keymap "\C-c\pd" 'emacs-goto-definition)
    keymap))

;;; Internal project functions.

(defun emacs--form-core (name keyword)
  (interactive "sCore name: \nsKeywords: ")
  (emacs--create-action "core" name keyword))

(defun emacs--form-component (name keyword)
  (interactive "sComponent name: \nsKeywords: ")
  (emacs--create-action "component" name keyword))

(defun emacs--get-components ()
  (let ((components))
    (fetch-content embla-component-directory
      (when (and (file-directory-p path)
                (not (equal f "."))
                (not (equal f "..")))
        (add-to-list 'components f)))
    components))

(defun emacs--get-package-under-cursor ()
  (kill-new
    (save-mark-and-excursion
      (when (and (not (or (> (nth 0 (syntax-ppss)) 0)
                          (nth 3 (syntax-ppss))))
                 (not (equal (char-after) ?\()))
        (error "This function only works on 'require-package' statement."))
      (let ((start) (text))
        ;; Go back to '(' character if it's not already on it.
        (when (not (equal (char-after) ?\())
          (backward-up-list))

        (forward-char)
        (setq start (point))
        (skip-chars-forward "^\)")
        (setq text (buffer-substring-no-properties start (point)))

        (if (not (string-match "^require-package " text))
          (error "This function only works on 'require-package' statement.")
          (setq text (replace-regexp-in-string "require-package " "" text))
          (save-match-data
            (and (string-match "[A-Za-z0-9\-]+" text)
              (match-string 0 text))))))))

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
         (path-destination)
         (path-validation)
         (references)
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

        ;; Replace content in destination file.
        (replace-in-file path
          '((cons "__content__" content)))

        ;; Replace somes variables in destination file.
        (replace-in-file path
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
  (interactive)
  (ivy-read "Create Template: "
    '("component" "core")
    :require-match t
    :preselect emacs--last-selection
    :action (lambda (target)
      (setq emacs--last-selection target)
      (when-function-exists (concat "emacs--form-" target)
        (setq emacs--last-selection target)
        (call-interactively func)))))

(defun emacs-goto-definition ()
  "Go to package definition in component directory."
  (interactive)
  (let ((type-definition) (current-path) (config-path) (module) (package) (line))
    ;; Define type of definition and current module by its buffer path.
    ;; Expected all subdirectories in 'component' and 'language'.
    (setq current-path (substring buffer-file-name (length (projectile-project-root))))
    (let* ((parts (split-string current-path "\/"))
           (directory (nth 0 parts)))
      (if (and (string-equal (nth 0 parts) "component")
               (string-equal (nth 2 parts) "packages.el"))
        (setq type-definition (nth 0 parts)
              module (nth 1 parts)
              config-path (concat (projectile-project-root)
                                  type-definition "/" module "/config.el"))
        (error "This function only works in 'component' directory.")))

    (setq package (emacs--get-package-under-cursor))
    (with-temp-buffer
      (insert-file-contents config-path)
      (let ((func (concat "defun " module "/init-" package)))
        (if (not (string-match func (buffer-string)))
          (error "This package doesn't have any configuration function.")
          (search-forward (concat "defun " module "/init-" package))
          (setq line (line-number-at-pos)))))

    (find-file config-path)
    (goto-line line)))

(defun emacs-reload ()
  "Reload init configuration."
  (interactive)
  (load-file embla-core-init)
  (project-enable-minor-mode))

;;; Define minor mode.

(define-minor-mode emacs-mode
  "A minor-mode to help to manage Emacs configuration."
  nil " emacs" emacs-mode-map)
