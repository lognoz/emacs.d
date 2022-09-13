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
(embla-autoload "dired" dired-mode-hook)

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
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (dired-hide-details-mode t)

  (let ((map dired-mode-map))
    (define-key map (kbd "RET") #'dired-find-alternate-file)
    (define-key map (kbd "<mouse-2>") #'dired-find-alternate-file)
    (define-key map (kbd "<s-tab>") #'dired-hide-details-mode)
    (define-key map (kbd "<M-tab>") #'dired-switch-dotfile)
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "C-_") #'dired-undo)
    (define-key map (kbd "S") #'embla-dired-do-slugify)
    (define-key map (kbd "F") #'embla-dired-open-marked)
    (define-key map (kbd "o") #'dired-find-file-other-window)
    (define-key map (kbd "/f") #'embla-dired-grep-file-name)
    (define-key map (kbd "/g") #'embla-dired-grep-file-name-by-pattern)))


;;;; --- Dired utilities

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
