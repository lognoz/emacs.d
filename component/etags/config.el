;;; config.el - Etags Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: etags

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

(defvar etags-in-process nil)

(defvar etags-projects nil)

(defvar etags-last-build nil)

(add-to-list 'display-buffer-alist
  (cons "\\*ctags\\*.*" (cons #'display-buffer-no-window nil)))

(defun etags/after-save-hook ()
  (when (and (not etags-in-process) (projectile-project-root))
    (let* ((project-root (projectile-project-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command tags-file tags-exclude default-directory))
           (timestamp (- (float-time (current-time)) (float-time etags-last-build))))
      (when (or (eq etags-last-build nil)
                (eq etags-projects nil)
                (eq (member default-directory etags-projects) nil)
                (> timestamp 100))
        (setq etags-last-build (current-time))
        (add-to-list 'etags-projects default-directory)
        (etags/async-process command)))))

(defun etags/async-process (command)
  (setq etags-in-process t)
  (when (get-buffer "*ctags*")
    (kill-buffer "*ctags*"))
  (let* ((output-buffer (generate-new-buffer "*ctags*"))
         (proc (progn
          (async-shell-command command output-buffer)
          (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'etags/stop-process)
      (message "No process running."))))

(defun etags/stop-process (process signal)
  (when (memq (process-status process) '(exit signal))
    (setq etags-in-process nil)
    (message (concat "Generate " (projectile-project-root) "TAGS"))))
