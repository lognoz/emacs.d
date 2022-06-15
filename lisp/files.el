;;; files.el --- Extensions to files -*- lexical-binding: t -*-

;; Copyright (C)  Marc-Antoine Loignon <developer@lognoz.org>

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/embla
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an extension for file and directory-handling functions,
;; backup generation, etc.

;;; Code:

;;;###autoload
(embla-autoload "files" after-find-file dired-initial-position-hook)

;;;; Unique buffer names

(embla-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;; Recentf support

(defconst embla-recentf-file (expand-file-name "recentf" embla-temporary-directory)
  "The recentf file.")

(embla-builtin-package 'recentf
  (setq recentf-save-file embla-recentf-file)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 200)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (recentf-mode t))

;;;; Savehist support

(embla-builtin-package 'savehist
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

;;;; Backup support

(defconst embla-backup-session-directory (expand-file-name "backup/session" embla-temporary-directory)
  "The directory of backup files per session.")

(defconst embla-backup-save-directory (expand-file-name "backup/save" embla-temporary-directory)
  "The directory of backup files on save.")

(embla-builtin-package 'files
  (setq auto-save-interval 20)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq kept-new-versions 8)
  (setq kept-old-versions 0)
  (setq vc-make-backup-files t)
  (setq version-control t)

  (let ((path embla-backup-session-directory))
    (setq backup-directory-alist `((".*" . ,path))))

  (add-hook 'before-save-hook #'embla-files--save-backup-file))

(defun embla-files--save-backup-file ()
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

;;;; Trashed support

(embla-elpa-package 'trashed
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;;; Contextual finder

(autoload 'find-file-at-point "ffap")
(autoload 'ffap-file-at-point "ffap")
(autoload 'ffap-string-at-point "ffap")

;;;###autoload (define-key global-map (kbd "C-x f") #'embla-find-contextual-file)
;;;###autoload
(defun embla-find-contextual-file ()
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
          (t (find-file root)))))

;;; files.el ends here
