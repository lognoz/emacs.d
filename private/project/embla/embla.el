;;; embla.el --- Embla Project File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: embla project

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

(defvar embla-last-template-selection nil)

(defvar embla-template-directory (concat embla-project-directory "embla/template/")
  "The directory of template files.")

;;;###autoload
(defvar embla-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\pc" 'embla-create-template)
    (define-key keymap "\C-c\pl" 'embla-reload-init)
    (define-key keymap "\C-c\pd" 'embla-goto-definition)
    keymap))

;;; Internal project functions.

(defun embla-trim-value (name value)
  "Trim anwser value and check if `name' and `keyword'
variable is empty."
  (setq value (string-trim value))
  (when (string-equal value "")
    (error (concat "Error: " name " can't be empty.")))
  value)

(defun embla-check-if-file-exists (type path filename)
  "Check if there is already a composite with that name."
  (dolist (f (directory-files path))
    (when (string-equal filename f)
      (error (concat "Error: There is already a " type " directory with that name.")))))

(defun embla-create-core-template (name keyword)
  "Interactive function to create core template."
  (interactive "sCore name: \nsKeywords: ")
  ;; Trim name and keyword.
  (setq name (embla-trim-value "name" name)
        keyword (embla-trim-value "keyword" keyword))
  (let* ((slugify-name (replace-regexp-in-string " " "-" (downcase name)))
         (filename (concat "core-" slugify-name ".el"))
         (path (expand-file-name filename embla-core-directory)))
    ;; Check if the file exist in core directory.
    (embla-check-if-file-exists "core" embla-core-directory filename)
    ;; Copy and replace varaibles.
    (copy-file (expand-file-name "header" embla-template-directory) path)
    ;; Replace somes variables in destination file.
    (replace-in-file path
      '((cons "__title__" (concat (capitalize name) " Initialization"))
        (cons "__file__" filename)
        (cons "__name__" (capitalize name))
        (cons "__keyword__" (downcase keyword))))
    ;; Open file and go to the end of the buffer.
    (find-file path)
    (end-of-buffer)))

(defun embla-create-component-template (name keyword)
  "Interactive function to create component template."
  (interactive "sComponent name: \nsKeywords: ")
  ;; Trim name and keyword.
  (setq name (embla-trim-value "name" name)
        keyword (embla-trim-value "keyword" keyword))
  (let* ((slugify-name (replace-regexp-in-string " " "-" (downcase name)))
         (component (concat embla-component-directory slugify-name))
         (files-to-create '("config.el" "package.el")))
    ;; Check if the directory exist in core directory.
    (embla-check-if-file-exists "component" embla-component-directory slugify-name)
    ;; Create component directory.
    (make-directory component)
    ;; Replace somes variables in destination file.
    (dolist (f files-to-create)
      (let ((path (expand-file-name f component)))
        (copy-file (expand-file-name "header" embla-template-directory) path)
        ;; Replace somes variables in destination file.
        (replace-in-file path
          '((cons "__title__"
              (concat (capitalize name)
                      (cond ((string-equal f "config.el") " Component")
                            ((string-equal f "package.el") " Package Component"))))
            (cons "__file__" f)
            (cons "__name__" (capitalize name))
            (cons "__keyword__" (downcase keyword))))
        ;; Open file and go to the end of the buffer.
        (when (string-equal f (car (last files-to-create)))
          (find-file path)
          (end-of-buffer))))))

(defun embla-verify-statement ()
  "Throw error if the cursor is not in a statement."
  (when (and (not (or (> (nth 0 (syntax-ppss)) 0)
                      (nth 3 (syntax-ppss))))
             (not (equal (char-after) ?\()))
    (error "This function only works on 'require-package' statement.")))

(defun embla-package-under-cursor ()
  "Return package name in a require-package statement."
  (kill-new
    (save-mark-and-excursion
      (embla-verify-statement)
      (let ((start) (text))

        ;; Go back to '(' character if it's not already on it.
        (when (not (equal (char-after) ?\())
          (backward-up-list))

        ;; Get text under cursor.
        (forward-char)
        (setq start (point))
        (skip-chars-forward "^\)")
        (setq text (buffer-substring-no-properties start (point)))

        ;; Verify if text under cursor is a require-package statement
        ;; and return the matching string.
        (if (not (string-match "^require-package " text))
          (error "This function only works on 'require-package' statement.")
          (setq text (replace-regexp-in-string "require-package " "" text))
          (save-match-data
            (and (string-match "[A-Za-z0-9\-]+" text)
              (match-string 0 text))))))))

(defun embla-reference-path (split-path)
  "Return the config path of component by a splited url.
Expected package.el file in component directory."
  (if (and (string-equal (nth 0 split-path) "component")
            (string-equal (nth 2 split-path) "package.el"))
    (concat (projectile-project-root)
      "component/" (nth 1 split-path) "/config.el")
    (error "This function only works in 'component' directory.")))

;;; External project functions.

(defun embla-goto-definition ()
  "Go to package definition in component directory."
  (interactive)
  (let*
    ((path
      (substring buffer-file-name
        (length (projectile-project-root))))
     (split-path (split-string path "\/"))
     (module (nth 1 split-path))
     (reference (embla-reference-path split-path))
     (package (embla-package-under-cursor)))

    (with-temp-buffer
      (insert-file-contents reference)
      (let* ((func (concat "defun " module "/init-" package)))
        (if (not (string-match func (buffer-string)))
          (error "This package doesn't have any configuration function.")
          (search-forward (concat "defun " module "/init-" package))
          (setq line (line-number-at-pos)))))

    (find-file reference)
    (goto-line line)))

(defun embla-reload-init ()
  "Reload init configuration."
  (interactive)
  (load-file embla-core-init)
  (create-autoload-file t)
  (embla-after-startup-hook))

(defun embla-create-template ()
  "Create core or component template."
  (interactive)
  (ivy-read "Create Template: "
    '("component" "core")
    :require-match t
    :preselect embla-last-template-selection
    :action (lambda (target)
      (setq embla-last-template-selection target)
      (when-function-exists (concat "embla-create-" target "-template")
        (setq embla-last-template-selection target)
        (call-interactively func)))))

;;; Define minor mode.

;;;###autoload
(define-minor-mode embla-mode
  "A minor-mode to help to manage embla configuration."
  nil " embla" embla-mode-map)

(provide 'embla)
