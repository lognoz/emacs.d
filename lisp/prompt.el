;;; lisp/prompt.el --- Prompt utilities -*- lexical-binding: t -*-

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

(require 'project)

;;;###autoload
(embla-autoload "prompt" vterm-mode-hook)


;;; --- Prompt configurations

(embla-elpa-package 'vterm
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000))

(let ((map vterm-mode-map))
  (define-key map (kbd "C-z") #'embla-project-prompt)
  (define-key map (kbd "C-.") #'embla-prompt-goto-project-root)
  (define-key map (kbd "M-.") #'embla-prompt-goto-project-root)
  (define-key map (kbd "`") #'embla-prompt-execute-previous))

;;;###autoload
(defun embla-prompt-execute-previous ()
  "Execute previous prompt command."
  (interactive)
  (vterm-send-C-p)
  (vterm-send-return))

(defun embla-prompt-goto-project-root ()
  "Change the current location to project root on vterm."
  (interactive)
  (let* ((root (project-root (project-current t))))
    (when root
      (vterm-insert (concat "cd " root))
      (vterm-send-return))))

(defvar embla-prompt-commands '(("/usr/bin/" . nil))
  "List of lambda function added in
`embla-project-shell-command-after-hook' function.")

;;;###autoload
(defun embla-prompt-execute-command-after-save ()
  "Run shell command in `after-save-hook'."
  (interactive)
  (let* ((command (read-string "Shell command: "))
         (default-directory (read-directory-name "Working directory: "
                              (project-root (project-current t))))
         (func `(lambda (&rest _)
                 (when (and buffer-file-name
                         (string-match (concat "^" ,default-directory) buffer-file-name))
                   (shell-command ,command)))))
    (setq embla-prompt-commands
      (cons `(,default-directory . ,func) embla-prompt-commands))
    (message "Command '%s' is added into `after-save-hook'." command)
    (add-hook 'after-save-hook func)))

;;;###autoload
(defun embla-prompt-clear-command-after-save ()
  "Clear shell command in `after-save-hook'."
  (interactive)
  (let ((candidates (embla-prompt-clear-command-after-save--candidates)))
    (if candidates
        (setq embla-prompt-commands
          (assoc-delete-all (completing-read "Clear shell commands located in... " candidates nil t)
                            embla-prompt-commands))
      (message "No prompt command defined."))))

(defun embla-prompt-clear-command-after-save--candidates ()
  "Provide candidates to `embla-prompt-clear-command-after-save' prompt."
  (let ((candidates))
    (dolist (parameters embla-prompt-commands)
      (unless (member (car parameters) candidates)
        (push (car parameters) candidates)))
    candidates))

;;; prompt.el ends here
