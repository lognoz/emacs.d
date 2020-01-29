;;; core-func.el --- Core Function File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

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

;;;###autoload
(defun sudo ()
  "Use TRAMP to `sudo` the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;;;###autoload
(defun stage-current-buffer ()
  "Stage changes of active buffer."
  (interactive)
  (let* ((buffile (buffer-file-name))
    (output (shell-command-to-string
            (concat "git add " (buffer-file-name)))))
    (message (if (not (string= output ""))
        output (concat "Added " buffile)))))

;;;###autoload
(defun get-file-content (path)
  "Return the contents of filename."
  (string-trim
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;;###autoload
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

;;;###autoload
(defun byte-recompile (path)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory path)
    (byte-recompile-directory path)))

;;;###autoload
(defun recompile-elpa ()
  "Recompile packages in elpa directory."
  (interactive)
  (byte-recompile package-user-dir))

;;;###autoload
(defun recompile-embla ()
  "Recompile component and core directories."
  (interactive)
  (byte-recompile embla-core-directory)
  (byte-recompile embla-component-directory))

(provide 'core-func)
