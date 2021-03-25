;;; lisp/files.el --- files configurations -*- lexical-binding: t; -*-

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

;;;###autoload
(eval-before-init
  (require-package 'undo-tree)
  (require-package 'find-file-in-project))

;;;###autoload
(define-component embla-files (after-find-file dired-initial-position-hook)
  "Setup files component configurations."
  (setq bookmark-default-file (expand-file-name "bookmark" embla-temporary-directory))
  (setq save-place-file (expand-file-name "saveplace" embla-temporary-directory))
  (setup-backup)
  (setup-savehist)
  (setup-recentf)
  (save-place-mode t))

;;;###autoload
(bind-keys embla-mode-map
  ("C-x f" . find-contextual-file))

;;;###autoload
(autoloads
  ("ffap" find-file-at-point ffap-file-at-point ffap-string-at-point))

(defconst embla-recentf-file (expand-file-name "recentf" embla-temporary-directory)
  "The recentf file.")

(defconst embla-undo-directory (expand-file-name "undo" embla-temporary-directory)
  "The directory of undo files.")

(defconst embla-backup-session-directory (expand-file-name "backup/session" embla-temporary-directory)
  "The directory of backup files per session.")

(defconst embla-backup-save-directory (expand-file-name "backup/save" embla-temporary-directory)
  "The directory of backup files on save.")

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
  ;; Make a "per save" backup on each save. The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;;;###autoload
(defun setup-backup ()
  "Setup backup file and change `before-save-hook'."
  (setq auto-save-interval 20)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq kept-new-versions 8)
  (setq kept-old-versions 0)
  (setq vc-make-backup-files t)
  (setq version-control t)
  (let ((path embla-backup-session-directory))
    (setq backup-directory-alist `((".*" . ,path))))
  (add-hook 'before-save-hook 'save-backup-file))

;;;###autoload
(defun setup-savehist ()
  "Setup savehist configurations."
  (setq savehist-file (expand-file-name "savehist" embla-temporary-directory))
  (setq history-length 200)
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history))
  (savehist-mode t))

;;;###autoload
(defun setup-recentf ()
  "Setup recentf configurations."
  (setq recentf-save-file embla-recentf-file)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 200)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (recentf-mode t))

;;;###autoload
(defun setup-undo-tree ()
  "Setup undo tree configurations."
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        (list (cons "." embla-undo-directory)))
  (global-undo-tree-mode t))

;;;###autoload
(defun find-contextual-file ()
  "Find file under cursor.
If none found, it will prompt contextual `counsel-find-file'."
  (interactive)
  (let* ((file (ffap-file-at-point))
         (string (ffap-string-at-point))
         (root (ffip-project-root))
         (relative-path (expand-file-name (string-trim-left string "/") root)))
    (cond ((and file (file-exists-p file))
           (find-file file))
          ((and string (not (string-equal string "")) (file-exists-p relative-path))
           (find-file relative-path))
          (t (counsel-find-file root)))))

;;; files.el ends here
