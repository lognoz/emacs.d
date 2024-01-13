;;; lisp/shell.el --- Shell utilities -*- lexical-binding: t -*-

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

;;; Commentary:

;; This code provide useful shell abstraction.

;;; Code:

(require 'project)

(embla-eval-on-install
  (embla-elpa-package 'vterm)
  (vterm-module-compile))


;;; --- Shell configurations

;;;###autoload
(add-hook 'vterm-mode-hook #'embla-set-vterm-mode)

;;;###autoload
(defun embla-set-vterm-mode ()
  "Setup vterm component configurations."
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (let ((map vterm-mode-map))
    (define-key map (kbd "C-v") #'vterm-yank)
    (define-key map (kbd "C-z") #'embla-project-shell)
    (define-key map (kbd "C-.") #'embla-shell-goto-project-root)
    (define-key map (kbd "M-.") #'embla-shell-goto-project-root)
    (define-key map (kbd "C-s") #'embla-shell-execute-as-root)
    (define-key map (kbd "`") #'embla-shell-execute-previous)))

;;;###autoload
(defun embla-shell-execute-previous ()
  "Execute previous shell command."
  (interactive)
  (vterm-send-C-p)
  (vterm-send-return))

(defun embla-shell-execute-as-root ()
  "Execute last command executed as root."
  (interactive)
  (vterm-insert "doas !!")
  (vterm-send-return))

(defun embla-shell-goto-project-root ()
  "Change the current location to project root on vterm."
  (interactive)
  (let ((root (project-root (project-current t))))
    (when root
      (vterm-insert (concat "cd " root))
      (vterm-send-return))))

(defvar embla-shell-commands nil
  "List of commands executor based on working directory.")

;;;###autoload
(defun embla-shell-execute-command-after-save ()
  "Run shell command in `after-save-hook'."
  (interactive)
  (let* ((command (read-string "Shell command: "))
         (default-directory (read-directory-name "Working directory: "
                              (project-root (project-current t))))
         (func `(lambda (&rest _)
                 (when (and buffer-file-name
                         (string-match (concat "^" ,default-directory) buffer-file-name))
                   (shell-command ,command)))))
    (setq embla-shell-commands
      (cons `(,default-directory . ,func) embla-shell-commands))
    (message "Command '%s' is added into `after-save-hook'." command)
    (add-hook 'after-save-hook func)))

;;;###autoload
(defun embla-shell-clear-command-after-save ()
  "Clear shell command in `after-save-hook'."
  (interactive)
  (let ((candidates (embla-shell-clear-command-after-save--candidates)))
    (if candidates
        (setq embla-shell-commands
          (assoc-delete-all (completing-read "Clear shell commands located in... " candidates nil t)
                            embla-shell-commands))
      (message "No shell command defined."))))

(defun embla-shell-clear-command-after-save--candidates ()
  "Provide candidates to `embla-shell-clear-command-after-save' shell."
  (let ((candidates))
    (dolist (parameters embla-shell-commands)
      (unless (member (car parameters) candidates)
        (push (car parameters) candidates)))
    candidates))

;;; shell.el ends here
