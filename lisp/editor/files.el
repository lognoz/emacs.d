;;; lisp/editor/files.el --- files configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: files

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require-package 'undo-tree)

;;; --- Contextual constants

(defconst embla-savehist-file (expand-file-name "savehist" embla-temporary-directory)
  "The savehist file.")

(defconst embla-recentf-file (expand-file-name "recentf" embla-temporary-directory)
  "The recentf file.")

(defconst embla-saveplace-file (expand-file-name "saveplace" embla-temporary-directory)
  "The saveplace file.")

(defconst embla-bookmark-file (expand-file-name "bookmark" embla-temporary-directory)
  "The bookmark file.")

(defconst embla-undo-directory (expand-file-name "undo" embla-temporary-directory)
  "The directory of undo files.")

(defconst embla-backup-session-directory (expand-file-name "backup/session" embla-temporary-directory)
  "The directory of backup files per session.")

(defconst embla-backup-save-directory (expand-file-name "backup/save" embla-temporary-directory)
  "The directory of backup files on save.")


;;; --- Backup configuration

(use-package emacs
  :hook (before-save-hook . save-backup-file)
  :config
  (setq auto-save-interval 20)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq kept-new-versions 8)
  (setq kept-old-versions 0)
  (setq vc-make-backup-files t)
  (setq version-control t)

  (let ((path embla-backup-session-directory))
    (setq backup-directory-alist `((".*" . ,path))))

  (defun save-backup-file ()
    "Create a backup on each session and on save interval.
  This function is used as hook for `before-save-hook'."
    ;; Make a special "per session" backup at the first save of each
    ;; Emacs session.
    (when (not buffer-backed-up)
      ;; Override the default parameters for per-session backups.
      (let* ((path embla-backup-save-directory)
             (backup-directory-alist `((".*" . ,path)))
             (kept-new-versions 3))
        (backup-buffer)))
    ;; Make a "per save" backup on each save. The first save results
    ;; in both a per-session and a per-save backup, to keep the
    ;; numbering of per-save backups consistent.
    (let ((buffer-backed-up nil))
      (backup-buffer))))


;;; --- Savehist configuration

(use-package savehist
  :hook (after-init-hook . savehist-mode)
  :config
  (setq savehist-file embla-savehist-file)
  (setq history-length 200)
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)))


;;; --- Recentf configuration

(use-package recentf
  :config
  (setq recentf-save-file embla-recentf-file)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 200)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (recentf-mode t))


;;; --- Saveplace configuration

(use-package saveplace
  :config
  (setq save-place-file embla-saveplace-file)
  (save-place-mode t))


;;; --- Undo Tree configuration

(use-package undo-tree
  :hook (after-init-hook . global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        (list (cons "." embla-undo-directory))))


;;; --- Bookmark configuration

(setq bookmark-default-file embla-bookmark-file)


;;; files.el ends here
