;;; lisp/init-files.el --- files configurations -*- lexical-binding: t; -*-

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


;;; --- Internal functions

(defun file-save-backup ()
  "Hook function is used on `before-save-hook'.

It create a backup on each session and on save interval."
  ;; Make a special "per session" backup at the first save of each
  ;; Emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let* ((path embla-backup-save-directory)
           (backup-directory-alist `((".*" . ,path)))
           (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))


;;; --- Backup configuration

(setq auto-save-interval 20
      version-control t
      kept-new-versions 8
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t)

(let ((path embla-backup-session-directory))
  (setq backup-directory-alist `((".*" .  ,path))))

(add-hook 'before-save-hook 'file-save-backup)


;;; --- Savehist configuration

(setq savehist-file embla-savehist-file
      history-length 200
      savehist-autosave-interval 60
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history))
(savehist-mode t)


;;; --- Recentf configuration

(global-set-key (kbd "C-x f") 'recentf-open-files)

(setq recentf-save-file embla-recentf-file
      recentf-max-menu-items 10
      recentf-max-saved-items 200
      recentf-auto-cleanup 'never
      recentf-show-file-shortcuts-flag nil)

(recentf-mode t)


;;; --- Saveplace configuration

(setq save-place-file embla-saveplace-file)

(save-place-mode 1)


;;; --- Bookmark configuration

(setq bookmark-default-file embla-bookmark-file)


(provide 'init-files)

;;; init-files.el ends here
