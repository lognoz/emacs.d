;;; functionality-terminal.el --- Terminal Function File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: functionality terminal

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
(defun open-terminal ()
  "Open terminal on project root."
  (interactive)
  (let* ((root (projectile-project-root))
         (default-directory root)
         (buffers (cdr (buffer-list)))
         (project
           (if root (concat " - " (directory-name root)) ""))
         (terminal-name (concat "Terminal" project)))
    (while buffers
      (let ((buffer (car buffers)))
        (when (and (with-current-buffer buffer (string= "term-mode" major-mode))
                   (equal (buffer-name buffer) terminal-name))
          (switch-to-buffer buffer)
          (setq buffers nil))
        (setq buffers (cdr buffers))))
    (when (not (string= "term-mode" major-mode))
      (ansi-term "/bin/bash")
      (rename-buffer terminal-name))))

;;;###autoload
(defun open-terminal-other-window ()
  "Open terminal in other window."
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (other-window 1)
  (open-terminal))
