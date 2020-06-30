;;; core-file.el --- Core File Initialization File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: backup undo

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

;;; Internal core function.

(defun file-save-backup ()
  "Hook function is used on `before-save-hook'. It create a
backup on each session and on save interval."
  ;; Make a special "per session" backup at the first save of each
  ;; Emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let* ((path (concat embla-temporary-directory "backup/session"))
           (backup-directory-alist `((".*" . ,path)))
           (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;;; External core functions.

(defun file-set-undo-tree ()
  "Set undo tree module."
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
    (list (cons "." (expand-file-name "undo" embla-temporary-directory))))
  (global-undo-tree-mode 1))

(defun file-set-backup ()
  "Set Emacs backup files."
  ;; Save every 20 characters typed
  (setq auto-save-interval 20)
  ;; Use version numbers for backups.
  (setq version-control t)
  ;; Number of newest versions to keep.
  (setq kept-new-versions 8
        kept-old-versions 0)
  ;; Don't ask to delete excess backup versions.
  (setq delete-old-versions t)
  ;; Copy all files, don't rename them.
  (setq backup-by-copying t)
  ;; Backup also versioned files.
  (setq vc-make-backup-files t)
  ;; Default and save backups go here
  (let ((path (concat embla-temporary-directory "backup/save")))
    (setq backup-directory-alist `((".*" .  ,path))))
  ;; Create backup on save
  (add-hook 'before-save-hook 'file-save-backup))

(provide 'core-file)

;;; core-file.el ends here
