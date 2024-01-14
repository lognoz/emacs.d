;;; lisp/buffer.el --- Extensions to buffer -*- lexical-binding: t -*-

;; Copyright (c) Marc-Antoine Loignon <developer@lognoz.org>

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

;;; Code:

;;;###autoload
(fset 'yes-or-no-p 'y-or-n-p)

;;;###autoload
(let ((map embla-mode-map))
  (define-key map (kbd "s-1") #'delete-other-windows)
  (define-key map (kbd "s-2") #'split-window-below)
  (define-key map (kbd "s-3") #'split-window-right)
  (define-key map (kbd "s-o") #'other-window)
  (define-key map (kbd "s-`") #'embla-clone-buffer)
  (define-key map (kbd "s-k") #'embla-kill-current-buffer)
  (define-key map (kbd "s-f") #'embla-recently-closed-buffers-pop)
  (define-key map (kbd "s-<backspace>") #'embla-recently-closed-buffers-reopen)
  (define-key map (kbd "C-x `") #'embla-cloned-buffer-layout)
  (define-key map (kbd "C-x z") #'embla-project-shell-layout))

;; (defun embla-windows-context ()
;;   (let ((windows) (selected-window))
;;     (dolist (buffer (buffer-list))
;;       (when (get-buffer-window buffer 'visible)
;;         (push buffer windows)
;;         (when (eq buffer (window-buffer (selected-window)))
;;           (setq selected-window buffer))))
;;     (let ((context '()))
;;       (setq context (plist-put context :windows windows))
;;       (setq context (plist-put context :selected-window selected-window)))))

;;;###autoload
(defun embla-project-shell-layout ()
  "Close other window and open project shell on the right."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (embla-project-shell))

;;;###autoload
(defun embla-cloned-buffer-layout ()
  "Clone active buffer and switch to the right buffer."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;;;###autoload
(defun embla-rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name (format "New name for %s: "
                                              (file-name-nondirectory filename)))))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;###autoload
(defun embla-delete-current-buffer-file ()
  "Delete file related to the current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun embla-buffer-file-name-body (&optional filename)
  "Return the base name of the filename: no directory, no extension.
The default FILENAME is the `buffer-file-name'."
  (file-name-sans-extension
    (file-name-nondirectory (or filename (buffer-file-name)))))

;;;###autoload
(defun embla-clone-buffer ()
  "Clone next window buffer."
  (interactive)
  (switch-to-buffer (window-buffer (next-window (selected-window)))))

;;;###autoload
(defun embla-kill-current-buffer ()
  "Kill current buffer without asking confirmation."
  (interactive)
  (embla-recently-closed-buffers-new (current-buffer))
  (kill-buffer (buffer-name)))


;;; --- Ibuffer configurations

(embla-elpa-package 'ibuffer-project)

;;;###autoload
(add-hook 'ibuffer-mode-hook #'embla-set-ibuffer-mode)

;;;###autoload
(defun embla-set-ibuffer-mode ()
  "Setup ibuffer component configurations."
  (ibuffer-do-sort-by-alphabetic)
  (ibuffer-projectile-set-filter-groups)
  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (unless (eq ibuffer-sorting-mode 'project-file-relative)
    (ibuffer-do-sort-by-project-file-relative)))


;;; --- Manages buffers recently closed

(defvar embla-recently-closed-buffers-ring nil
  "List of recently closed buffers.")

(defcustom embla-recently-closed-buffers-max 100
  "Maximum length of recently closed buffers before oldest elements are thrown away."
  :type 'integer
  :group 'killing)

(defun embla-recently-closed-buffers-new (buffer)
  "Make BUFFER the latest kill in the recently closed buffers."
  (when (buffer-file-name buffer)
    (setq embla-recently-closed-buffers-ring (cons (cons (buffer-name buffer)
                                                         (buffer-file-name buffer))
                                                   embla-recently-closed-buffers-ring)))
  (when (> (length embla-recently-closed-buffers-ring) embla-recently-closed-buffers-max)
    (setq embla-recently-closed-buffers-ring (butlast embla-recently-closed-buffers-ring 1))))

(defun embla-recently-closed-buffers--candidates ()
  "Provide candidates to `embla-recently-closed-buffers-pop' prompt.
Return a list of buffers located into `embla-recently-closed-buffers-ring'."
  (mapcar (lambda (f) (cdr f))
          embla-recently-closed-buffers-ring))

(defmacro embla-recently-closed-buffers-restore (&rest body)
  "Restore given file if `embla-recently-closed-buffers-ring' is not empty."
  `(if (> (length embla-recently-closed-buffers-ring) 0)
      ,@body
    (message "No recently close buffer in this session.")))

;;;###autoload
(defun embla-recently-closed-buffers-pop (&optional arg)
  "Restore recently closed buffers."
  (interactive "p")
  (embla-recently-closed-buffers-restore
    (find-file (completing-read "Find file: " (embla-recently-closed-buffers--candidates) nil t))))

;;;###autoload
(defun embla-recently-closed-buffers-reopen (&optional arg)
  "Restore the last recently closed buffer."
  (interactive "p")
  (embla-recently-closed-buffers-restore
    (find-file (cdr (pop embla-recently-closed-buffers-ring)))))

;;; buffer.el ends here
