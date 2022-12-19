;;; lisp/dired.el --- Extensions to dired -*- lexical-binding: t -*-

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
(put 'dired-find-alternate-file 'disabled nil)

;;;###autoload
(let ((listing-switches (list "-ahl" "-v" "--group-directories-first")))
  (when (equal system-type 'berkeley-unix)
    (if-let (gls (executable-find "gls"))
            (setq insert-directory-program gls)
      (setq listing-switches (list (car listing-switches)))))
  (setq dired-listing-switches (string-join listing-switches " ")))

(embla-elpa-package 'dired-ranger)

(embla-elpa-package 'dired-subtree)

(embla-builtin-package 'dired
  (add-hook 'dired-mode-hook #'embla-set-dired-mode))

;;;###autoload
(embla-elpa-package 'dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'embla-set-dired-sidebar-mode))

;;;###autoload
(defun embla-set-dired-sidebar-mode ()
  "Setup dired sidebar component configurations."
  (unless (file-remote-p default-directory)
    (auto-revert-mode))

  (evil-collection-define-key 'normal 'dired-sidebar-mode-map
    (kbd "RET") #'embla-dired-subtree-or-find-alternate
    (kbd "<return>") #'embla-dired-subtree-or-find-alternate
    (kbd "<mouse-2>") #'embla-dired-subtree-or-find-alternate))

(defun embla-set-dired-mode ()
  "Setup dired component configurations."
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (dired-hide-details-mode t)

  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "RET") #'dired-find-alternate-file
    (kbd "<mouse-2>") #'dired-find-alternate-file
    (kbd "<s-tab>") #'dired-hide-details-mode
    (kbd "<M-tab>") #'dired-switch-dotfile
    (kbd "<tab>") #'dired-subtree-toggle
    (kbd "C-_") #'dired-undo
    (kbd "S") #'embla-dired-do-slugify
    (kbd "F") #'embla-dired-open-marked
    (kbd "o") #'dired-find-file-other-window
    (kbd "/f") #'embla-dired-grep-file-name
    (kbd "/g") #'embla-dired-grep-file-name-by-pattern))


;;;; --- Dired utilities

(defun embla-dired-subtree-or-find-alternate ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-subtree-toggle)
      (dired-sidebar-find-file))))

(defun embla-dired-switch-dotfile ()
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

(defun embla-dired-grep-file-name (name)
  "Search files in directory by name."
  (interactive "sFind-name (filename wildcard): ")
  (find-name-dired default-directory name))

(defun embla-dired-grep-file-name-by-pattern (pattern)
  "Search files in directory by PATTERN."
  (interactive "sFind-grep (grep regexp): ")
  (find-grep-dired default-directory pattern))

(defun embla-dired-open-marked ()
  "Open marked files in dired."
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

(defmacro embla-dired-fetch-marked-file (&rest body)
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

(defun embla-dired-do-slugify ()
  "Rename current file or all marked files."
  (interactive)
  (unless (executable-find "slugify")
    (user-error "Required program \"%s\" not found in your path" "slugify"))
  (if (equal (length (dired-get-marked-files)) 1)
      (embla-dired-fetch-marked-file
        (when (y-or-n-p (format "Rename to '%s'" new-name))
          (dired-rename-file marked-file new-name t)
          (revert-buffer)))
    (wdired-change-to-wdired-mode)
    (save-excursion
      (beginning-of-buffer)
      (embla-dired-fetch-marked-file
        (call-interactively 'dired-next-marked-file)
        (re-search-forward regexp nil t)
        (replace-match new-name)))
    (wdired-finish-edit)))

;;; dired.el ends here
