;;; core-files.el - Core Files Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: backup bookmark undo

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

;; Save every 20 characters typed
(setq auto-save-interval 20)

;; Bookmark file
(setq bookmark-default-file (concat embla-temporary-directory "bookmark"))

;; Save minibuffer
(savehist-mode 1)
(setq savehist-file (concat embla-temporary-directory "savehist"))
(setq history-length 100)

;; Record history
(recentf-mode 1)
(setq recentf-save-file (concat embla-temporary-directory "recentf"))
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 100)
(setq recentf-show-file-shortcuts-flag nil)

;; Save cursor positions
(save-place-mode 1)
(setq save-place-file (concat embla-temporary-directory "saveplace"))

;; Backup file
(setq version-control t       ; Use version numbers for backups.
      kept-new-versions 8     ; Number of newest versions to keep.
      kept-old-versions 0     ; Number of oldest versions to keep.
      delete-old-versions t   ; Don't ask to delete excess backup versions.
      backup-by-copying t     ; Copy all files, don't rename them.
      vc-make-backup-files t) ; Backup also versioned files.
;; Default and save backups go here
(let ((path (concat embla-temporary-directory "backup/save")))
  (setq backup-directory-alist `((".*" .  ,path))))
;; Create backup on save
(add-hook 'before-save-hook 'core-files//save-backup)

;;; Internal core functions.

(defun core-files//save-backup ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let* ((path (concat embla-temporary-directory "backup/session"))
           (backup-directory-alist `((".*" .  ,path)))
           (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;;; External core functions.

(defun core-files/embla-startup-hook ()
  ;; Undo file
  (packadd! undo-tree)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
    (list (cons "." (expand-file-name "undo" embla-temporary-directory))))
  (global-undo-tree-mode 1))
