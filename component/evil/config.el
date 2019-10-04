;;; config.el - Evil Component File

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

(defun evil/init-evil ()
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-mode 1)

  (define-key evil-normal-state-map (kbd "=") 'evil-indent-line)
  (define-key evil-normal-state-map (kbd "<") 'evil-shift-left-line)
  (define-key evil-normal-state-map (kbd ">") 'evil-shift-right-line)

  (define-key evil-visual-state-map (kbd "=") 'visual-indent)
  (define-key evil-visual-state-map (kbd "<") 'visual-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'visual-shift-right)

  (define-key evil-normal-state-map (kbd "C-j") (concat ":m .+1" (kbd "RET") "=="))
  (define-key evil-normal-state-map (kbd "C-k") (concat ":m .-2" (kbd "RET") "=="))

  (define-key evil-visual-state-map (kbd "C-j") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "C-k") (concat ":m '<-2" (kbd "RET") "gv=gv"))

  (defun visual-shift-left ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun visual-indent ()
    (interactive)
    (evil-indent (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun visual-shift-right ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore)))

(defun evil/init-evil-magit ()
  (with-eval-after-load 'magit
    (require 'evil-magit)))

(defun evil/init-evil-leader ()
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f"   'find-file
    "d"   'kill-this-buffer
    "w"   'save-buffer
    "ga"  'stage-current-buffer
    "gs"  'magit-status
    "gc"  'magit-commit-create
    "gl"  'magit-log-all
    "gps" 'magit-push-current-to-upstream))
