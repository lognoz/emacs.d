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

(defun replace-in-file (path replacements)
  "Replace variables in file."
  (with-temp-file path
    (insert-file-contents-literally path)
    (mapc (lambda (entry)
      (setq entry (eval entry))
      (goto-char 0)
      (while (search-forward (car entry) nil t)
        (replace-match (cdr entry))))
      replacements)))

(defun byte-recompile (path)
  "Byte compile a directory path."
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory path)
    (byte-recompile-directory path)))

(defun recompile-elpa ()
  "Recompile packages in elpa directory."
  (interactive)
  (byte-recompile package-user-dir))

(defun recompile-embla ()
  "Recompile component and core directories."
  (interactive)
  (byte-recompile embla-core-directory)
  (byte-recompile embla-component-directory))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

(defun open-terminal ()
  "Open terminal easily."
  (interactive)

  (let ((buffers (cdr (buffer-list))))
    (while buffers
      (when (with-current-buffer (car buffers) (string= "term-mode" major-mode))
        (switch-to-buffer (car buffers))
        (setq buffers nil))
      (setq buffers (cdr buffers)))

    (when (not (string= "term-mode" major-mode))
      (ansi-term "/bin/bash")
      (rename-buffer "Terminal"))))

(defun open-terminal-other-window ()
  "Open terminal in other window."
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (other-window 1)
  (open-terminal))

(provide 'core-func)
