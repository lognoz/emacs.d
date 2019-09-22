;;; configs.el - Evil Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: evil

;; This file is not part of GNU Emacs.

;; This Emacs config is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This Emacs config is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this Emacs config. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require-package (evil
                  evil-collection
                  evil-indent-plus
                  evil-leader
                  evil-magit
                  evil-smartparens
                  evil-surround))

(defun evil-initialize ()
  (evil--setup-leader)
  (global-evil-surround-mode 1)
  (evil-mode 1))

(defun evil--setup-leader ()
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f"   'find-file
    "d"   'kill-this-buffer
    "w"   'save-buffer
    "ga"  'stage-current-buffer
    "gs"  'magit-status
    "gc"  'magit-commit-create
    "gl"  'magit-log-all
    "gps" 'magit-push-current-to-upstream)

  (defun stage-current-buffer ()
    "Stage changes of active buffer."
    (interactive)
    (let* ((buffile (buffer-file-name))
      (output (shell-command-to-string
              (concat "git add " (buffer-file-name)))))
      (message (if (not (string= output ""))
          output (concat "Added " buffile))))))
