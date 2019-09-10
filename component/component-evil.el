;;; component-evil.el - Evil Component File

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

(defun component-evil/install ()
  "Install Evil requirements."
  (add-package (evil
                evil-collection
                evil-indent-plus
                evil-leader
                evil-magit
                evil-smartparens
                evil-surround)))

(defun component-evil/setup-evil ()
  "Configure evil mode."
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-mode 1))

(defun component-evil/setup-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f"   'find-file
    "d"   'kill-this-buffer
    "w"   'save-buffer
    "t"   'open-current-directory
    "ga"  'stage-current-buffer
    "gs"  'magit-status
    "gc"  'magit-commit-create
    "gl"  'magit-log-all
    "gps" 'magit-push-current-to-upstream)

  (defun open-current-directory ()
    (interactive)
    (dired (expand-file-name ".")))

  (defun stage-current-buffer ()
    "Stage changes of active buffer."
    (interactive)
    (let* ((buffile (buffer-file-name))
      (output (shell-command-to-string
              (concat "git add " (buffer-file-name)))))
      (message (if (not (string= output ""))
          output (concat "Added " buffile))))))

(component-evil/install)
(component-evil/setup-evil)
(component-evil/setup-leader)

(provide 'component-evil)
