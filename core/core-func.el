;;; core-func.el --- Core Function File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: functions helpers utils

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

(defun sudo ()
  "Use TRAMP to `sudo` the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))


;;; Eval and replace sexp.

(defun verify-statement (message)
  "Throw error if the cursor is not in a statement."
  (when (and (not (or (> (nth 0 (syntax-ppss)) 0)
                      (nth 3 (syntax-ppss))))
             (not (equal (char-after) ?\()))
    (error message)))

(defun sexp-insert-mode ()
  (if (or (equal evil-state 'normal)
          (equal evil-state 'insert))
    (evil-insert-state)
    (error "This function can only be executed in normal or insert mode.")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (sexp-insert-mode)
  (verify-statement "This function only works on lisp sexp.")
  (when (not (equal (char-after) ?\())
    (backward-up-list))
  (forward-sexp)
  (backward-kill-sexp)
  (condition-case nil
    (prin1 (eval (read (current-kill 0)))
          (current-buffer))
    (error (message "Invalid expression")
          (insert (current-kill 0)))))

;;; Find file on cursor.

(defun content-inner-quote (char)
  (let* ((right (save-excursion
                  (re-search-forward char nil t)))
         (left (save-excursion
                  (re-search-backward char nil t))))
    (when (and right right)
      (substring (buffer-substring-no-properties right left) 1 -1))))

(defun find-file-on-cursor ()
  (interactive)
  (let ((root (projectile-project-root))
        (content (content-inner-quote "\"")))
    (unless (file-exists-p (concat root content))
      (setq content (content-inner-quote "'")))
    (if (file-exists-p (concat root content))
      (find-file (concat root content))
      (error "Could not find file on point."))))

(provide 'core-func)
