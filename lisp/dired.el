;;; lisp/dired.el --- dired configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: dired

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
  (require-package 'dired-ranger)
  (require-package 'dired-subtree))

;;;###autoload
(define-component embla-dired (dired-mode-hook)
  "Setup dired component configurations."
  (setup-dired)
  (dired-hide-details-mode t))

;;;###autoload
(put 'dired-find-alternate-file 'disabled nil)

;;;###autoload
(let ((listing-switches (list "-ahl" "-v" "--group-directories-first")))
  (when bsd-p
    (if-let (gls (executable-find "gls"))
            (setq insert-directory-program gls)
      (setq listing-switches (list (car listing-switches)))))
  (setq dired-listing-switches (string-join listing-switches " ")))

;;;###autoload
(defun setup-dired ()
  "Setup dired configurations."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "RET") 'dired-find-alternate-file
    (kbd "<mouse-2>") 'dired-find-alternate-file
    (kbd "<s-tab>") 'dired-hide-details-mode
    (kbd "<M-tab>") 'dired-switch-dotfile
    (kbd "<tab>") 'dired-subtree-toggle
    (kbd "C-_") 'dired-undo
    "S"  'dired-do-slugify
    "F"  'dired-open-marked
    "o"  'dired-find-file-other-window
    "/f" 'dired-grep-file-name
    "/g" 'dired-grep-file-name-by-pattern))

(defun dired-switch-dotfile ()
  "Switch visibility of dotfiles lines."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun dired-grep-file-name (name)
  "Search files in directory by name."
  (interactive "sFind-name (filename wildcard): ")
  (find-name-dired default-directory name))

(defun dired-grep-file-name-by-pattern (pattern)
  "Search files in directory by PATTERN."
  (interactive "sFind-grep (grep regexp): ")
  (find-grep-dired default-directory pattern))

(defun dired-open-marked ()
  "Open marked files in dired."
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

(defmacro dired-fetch-marked-file (&rest body)
  "Fetch marked files and execute `slugify'."
  `(mapc (lambda (marked-file)
           (let* ((extension (file-name-extension marked-file))
                  (dired-file (file-name-nondirectory marked-file))
                  (regexp (regexp-quote dired-file))
                  (file-name
                    (if (not (file-directory-p marked-file))
                        (file-name-sans-extension dired-file)
                      dired-file))
                  (new-name
                    (concat (string-trim (shell-command-to-string (format "slugify \"%s\"" file-name)))
                            (when (and (not (file-directory-p marked-file)) extension)
                              (concat "." extension)))))
             ,@body))
         (dired-get-marked-files)))

(defun dired-do-slugify ()
  "Rename current file or all marked files."
  (interactive)
  (require-program "slugify")
  (if (equal (length (dired-get-marked-files)) 1)
      (dired-fetch-marked-file
        (setq new-name (read-file-name "Rename to: " new-name))
        (dired-rename-file marked-file new-name t)
        (revert-buffer))
    (wdired-change-to-wdired-mode)
    (save-excursion
      (beginning-of-buffer)
      (dired-fetch-marked-file
        (call-interactively 'dired-next-marked-file)
        (re-search-forward regexp nil t)
        (replace-match new-name)))
    (wdired-finish-edit)))

;;; dired.el ends here
