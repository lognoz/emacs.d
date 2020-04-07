;;; functionality-editing.el --- Editing Function File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: functionality editing content

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

(defun check-lisp-statement ()
  "Check if the cursor is under lisp expression."
  (when (and (not (or (> (nth 0 (syntax-ppss)) 0)
                      (nth 3 (syntax-ppss))))
             (not (equal (char-after) ?\()))
    (error "Need to be executed on lisp expression.")))

(defun check-evil-state ()
  "Check evil state before to replace the exepression by its
value. If it's in normal mode, enter in insert."
  (if (or (equal evil-state 'normal)
          (equal evil-state 'insert))
    (evil-insert-state)
    (error "Need to be executed in normal or insert mode.")))

(defun inner-character-content (char)
  "Return content inner a character."
  (let* ((right (save-excursion
                  (re-search-forward char nil t)))
         (left (save-excursion
                  (re-search-backward char nil t))))
    (when (and right right)
      (substring (buffer-substring-no-properties right left) 1 -1))))

;;;###autoload
(defun get-file-content (path)
  "Return contents of filename."
  (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;;###autoload
(defun replace-in-file (path replacement)
  "Replace variables in file."
  (with-temp-file path
    (insert-file-contents-literally path)
    (mapc (lambda (entry)
      (setq entry (eval entry))
      (goto-char 0)
      (while (search-forward (car entry) nil t)
        (replace-match (cdr entry))))
      replacement)))

;;;###autoload
(defun replace-in-string (string replacement)
  "Replace variables in string."
  (mapc (lambda (entry)
    (setq entry (eval entry))
    (setq string
      (replace-regexp-in-string
        (car entry) (cdr entry) string nil 'literal)))
    replacement)
  string)

;;;###autoload
(defun recursive-directories (path)
  "Return all directories in a specific path."
  (split-string
    (shell-command-to-string
      (concat "find " path " -type d"))
    "\n" t))

;;;###autoload
(defun eval-and-replace ()
  "Replace lisp statement by its value. This functionality can
only be executed on normal and insert mode."
  (interactive)
  (check-evil-state)
  (check-lisp-statement)
  (when (not (equal (char-after) ?\())
    (backward-up-list))
  (forward-sexp)
  (backward-kill-sexp)
  (condition-case nil
    (prin1 (eval (read (current-kill 0)))
          (current-buffer))
    (error (message "Invalid expression")
          (insert (current-kill 0)))))

;;;###autoload
(defun find-file-on-cursor ()
  "Switch to a buffer visiting file by string under cursor."
  (interactive)
  (let ((root (projectile-project-root))
        (content (inner-character-content "\"")))
    (unless (file-exists-p (concat root content))
      (setq content (inner-character-content "'")))
    (if (file-exists-p (concat root content))
      (find-file (concat root content))
      (error "Could not find file on point."))))

(provide 'functionality-editing)
