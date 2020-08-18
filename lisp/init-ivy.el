;;; lisp/init-ivy.el --- ivy configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: ivy

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

(require-package 'counsel
                 'ivy
                 'ivy-prescient
                 'prescient)

;;; --- External functions

(defun ivy-resize-minibuffer-setup-hook ()
  "Minibuffer setup hook."
  (add-hook 'post-command-hook #'ivy-resize-post-commad-hook nil t))

(defun ivy-resize-post-commad-hook ()
  "Hook run every command in minibuffer."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))


;;; --- Ivy configuration

(setq enable-recursive-minibuffers t
      ivy-initial-inputs-alist nil
      ivy-count-format "(%d/%d) "
      ivy-wrap nil
      ivy-display-style 'fancy
      ivy-use-selectable-prompt t
      ivy-fixed-height-minibuffer nil)

;; Change the maximum width of the Ivy window to 1/3
(setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))
(add-hook 'minibuffer-setup-hook 'ivy-resize-minibuffer-setup-hook)

(ivy-mode t)


;;; --- Counsel configuration

(setq counsel-find-file-at-point t
      counsel-yank-pop-preselect-last t
      counsel-yank-pop-separator "\n────────\n")

(setq counsel-find-file-ignore-regexp
      (concat
        ;; File names beginning with # or .
        "\\(?:\\`[#.]\\)"
        ;; File names ending with # or ~
        "\\|\\(?:\\`.+?[#~]\\'\\)"))


;;; --- Ivy keybindings

(set-keybinding
  :map ivy-minibuffer-map
  :define
  '("TAB" ivy-alt-done))

(set-keybinding
  :map embla-mode-map
  :define
  '("M-x"      counsel-M-x
    "M-y"      counsel-yank-pop
    "C-x d"    counsel-dired
    "C-x f"    counsel-recentf
    "C-x C-f"  counsel-find-file
    "C-c C-j"  counsel-imenu
    "C-x r l"  counsel-bookmark))


(provide 'init-ivy)

;;; init-ivy.el ends here
