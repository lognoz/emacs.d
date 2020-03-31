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

(defun get-file-content (path)
  "Return the contents of filename."
  (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

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

(defun replace-in-string (string replacement)
  (mapc (lambda (entry)
    (setq entry (eval entry))
    (setq string
      (replace-regexp-in-string
        (car entry) (cdr entry) string nil 'literal)))
    replacement)
  string)

;; (defun replace-in-string (regexp rep string)
;;   ;;(mapc (lambda (entry)
;;   (replace-regexp-in-string regexp rep string nil 'literal))

(defun recursive-directories (path)
  "Return all directories in a specific path."
  (split-string
    (shell-command-to-string
      (concat "find " path " -type d"))
    "\n" t))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))










;; (defun byte-recompile (path)
;;   "Byte compile a directory path."
;;   (if (fboundp 'async-byte-recompile-directory)
;;       (async-byte-recompile-directory path)
;;     (byte-recompile-directory path)))

;; (defun recompile-elpa ()
;;   "Recompile packages in elpa directory."
;;   (interactive)
;;   (byte-recompile package-user-dir))

;; (defun recompile-embla ()
;;   "Recompile component and core directories."
;;   (interactive)
;;   (byte-recompile embla-core-directory)
;;   (byte-recompile embla-component-directory))


;; (defun open-terminal ()
;;   "Open terminal easily."
;;   (interactive)
;;   (let ((buffers (cdr (buffer-list))))
;;     (while buffers
;;       (when (with-current-buffer (car buffers) (string= "term-mode" major-mode))
;;         (switch-to-buffer (car buffers))
;;         (setq buffers nil))
;;       (setq buffers (cdr buffers)))
;;     (when (not (string= "term-mode" major-mode))
;;       (ansi-term "/bin/bash")
;;       (rename-buffer "Terminal"))))

;; (defun open-terminal-other-window ()
;;   "Open terminal in other window."
;;   (interactive)
;;   (when (one-window-p)
;;     (split-window-right))
;;   (other-window 1)
;;   (open-terminal))

;; ;;; TODO: Kill buffer on same directory.

;; (defun kill-buffers-by-directory-action (path)
;;   (interactive
;;     (list (read-directory-name
;;             "Kill buffers by directory: "
;;             (file-name-directory buffer-file-name))))
;;   (let ((buffers (cdr (buffer-list)))
;;         (buffers-to-kill))
;;     (while buffers
;;       (let ((file-name (buffer-file-name (car buffers))))
;;         (when file-name
;;           (prin1 (car buffers))))
;;           ;;(prin1 (string-match path (file-name-directory file-name)))))
;;       (setq buffers (cdr buffers)))))

;; (defun kill-buffers-by-directory ()
;;   (interactive)
;;   (if (buffer-file-name)
;;     (call-interactively 'kill-buffers-by-directory-action)
;;     (error "Buffer '%s' is not visiting a file!" (buffer-name))))

;; ;;; Eval and replace sexp.

;; (defun verify-statement (message)
;;   "Throw error if the cursor is not in a statement."
;;   (when (and (not (or (> (nth 0 (syntax-ppss)) 0)
;;                       (nth 3 (syntax-ppss))))
;;              (not (equal (char-after) ?\()))
;;     (error message)))

;; (defun sexp-insert-mode ()
;;   (if (or (equal evil-state 'normal)
;;           (equal evil-state 'insert))
;;     (evil-insert-state)
;;     (error "This function can only be executed in normal or insert mode.")))

;; (defun eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (sexp-insert-mode)
;;   (verify-statement "This function only works on lisp sexp.")
;;   (when (not (equal (char-after) ?\())
;;     (backward-up-list))
;;   (forward-sexp)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;     (prin1 (eval (read (current-kill 0)))
;;           (current-buffer))
;;     (error (message "Invalid expression")
;;           (insert (current-kill 0)))))

;; ;;; Find file on cursor.

;; (defun content-inner-quote (char)
;;   (let* ((right (save-excursion
;;                   (re-search-forward char nil t)))
;;          (left (save-excursion
;;                   (re-search-backward char nil t))))
;;     (when (and right right)
;;       (substring (buffer-substring-no-properties right left) 1 -1))))

;; (defun find-file-on-cursor ()
;;   (interactive)
;;   (let ((root (projectile-project-root))
;;         (content (content-inner-quote "\"")))
;;     (unless (file-exists-p (concat root content))
;;       (setq content (content-inner-quote "'")))
;;     (if (file-exists-p (concat root content))
;;       (find-file (concat root content))
;;       (error "Could not find file on point."))))

(provide 'core-func)
