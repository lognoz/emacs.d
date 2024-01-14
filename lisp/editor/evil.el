;;; list/editor/evil.el --- Extensions to evil -*- lexical-binding: t -*-

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

;;; Code:

;;;###autoload
(embla-autoload "editor/evil" emacs-startup-hook)



;;;; --- Evil configurations

(defvar embla-evil-collection-mode-list
  '(apropos
    bookmark
    calc
    calendar
    company
    debug
    dired
    help
    ibuffer
    magit
    ag)
  "The list of `evil-collection' modes to be loaded automatically.")

(setq evil-want-keybinding nil)

(embla-elpa-package 'evil
  (setq evil-toggle-key "C-`")

  (evil-mode t)

  ;; Fix paste when selected.
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-kill-on-visual-paste nil)

  (let ((map evil-normal-state-map))
    (define-key map (kbd "j") #'evil-next-visual-line)
    (define-key map (kbd "k") #'evil-previous-visual-line)
    (define-key map (kbd "=") #'evil-indent-line)
    (define-key map (kbd "<") #'evil-shift-left-line)
    (define-key map (kbd ">") #'evil-shift-right-line)

    ;; Remove the vim way to reundo to keep native Emacs backward
    ;; search. Use C-? to reundo instead.
    (define-key map (kbd "C-r") nil)

    ;; Remove the vim way to start a macro. Use Emacs binding instead.
    (define-key map (kbd "q") nil))

  (let ((map evil-motion-state-map))
    (define-key map (kbd "!") #'shell-command)
    (define-key map (kbd ":") #'embla-goto-line)
    (define-key map (kbd "+") #'embla-goto-forward-line)
    (define-key map (kbd "-") #'embla-goto-backward-line)

    ;; Remove evil search and some motions.
    (define-key map (kbd "{") nil)
    (define-key map (kbd "}") nil)
    (define-key map (kbd "/") nil)
    (define-key map (kbd "*") nil)

    ;; Remove bindings because it's already reserved.
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-b") nil)

    ;; Remove C-y binding because it's already reserved to paste
    ;; kill ring.
    (define-key map (kbd "C-y") nil)

    ;; Remove the evil way to consult a manual. Use C-h M instead.
    (define-key map (kbd "K") nil))

  (let ((map evil-emacs-state-map))
    ;; Remove bindings because it's already reserved.
    (define-key map (kbd "C-z") nil))

  (let ((map evil-visual-state-map))
    (define-key map (kbd "=") #'embla-evil-visual-indent)
    (define-key map (kbd "<") #'embla-evil-visual-shift-left)
    (define-key map (kbd ">") #'embla-evil-visual-shift-right))

  (evil-set-initial-state 'vterm-mode 'emacs))

(embla-elpa-package 'evil-indent-plus
  (let ((map evil-inner-text-objects-map))
    (define-key map (kbd "i") #'evil-indent-plus-i-indent)
    (define-key map (kbd "I") #'evil-indent-plus-i-indent-up)
    (define-key map (kbd "J") #'evil-indent-plus-i-indent-up-down))

  (let ((map evil-outer-text-objects-map))
    (define-key map (kbd "i") #'evil-indent-plus-i-indent)
    (define-key map (kbd "I") #'evil-indent-plus-i-indent-up)
    (define-key map (kbd "J") #'evil-indent-plus-i-indent-up-down)))

(embla-elpa-package 'evil-smartparens)

(embla-elpa-package 'evil-collection
  (dolist (mode embla-evil-collection-mode-list)
    (with-eval-after-load mode
      (evil-collection-init (list mode)))))

(embla-elpa-package 'evil-surround
  (global-evil-surround-mode t))

(defun embla-evil-visual-indent (beg end)
  "Indent text form BEG to END."
  (interactive "r")
  (evil-indent beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun embla-evil-visual-shift-left (beg end)
  "Shift text from BEG to END to the left."
  (interactive "r")
  (evil-shift-left beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun embla-evil-visual-shift-right (beg end)
  "Shift text from BEG to END to the right."
  (interactive "r")
  (evil-shift-right beg end)
  (evil-normal-state)
  (evil-visual-restore))

(defun embla-goto-line (line &optional buffer relative)
  ""
  (interactive
    (progn
      (setq display-line-numbers t)
      (goto-line-read-args (buffer-narrowed-p))))
  (setq display-line-numbers 'relative)
  (goto-line line buffer relative))

(defun embla-goto-backward-line (line &optional buffer relative)
  ""
  (interactive (goto-line-read-args (buffer-narrowed-p)))
  (setq line (- (line-number-at-pos) line))
  (goto-line line buffer (buffer-narrowed-p)))

(defun embla-goto-forward-line (line &optional buffer relative)
  ""
  (interactive (goto-line-read-args (buffer-narrowed-p)))
  (setq line (+ (line-number-at-pos) line))
  (goto-line line buffer (buffer-narrowed-p)))

;;; evil.el ends here
