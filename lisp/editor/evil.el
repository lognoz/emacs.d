;;; lisp/editor/evil.el --- evil configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: evil

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

(require-package 'evil
                 'evil-magit
                 'evil-indent-plus
                 'evil-smartparens
                 'evil-surround
                 'evil-collection)


(defvar evil-collection-mode-list
  '(ag
    apropos
    bookmark
    calc
    calendar
    company
    debug
    dired
    ibuffer)
  "The list of `evil-collection' modes to be loaded automatically.")

;;;###autoload
(defun evil-initialize ()
  "Sets evil configurations."
  (setq evil-toggle-key "C-`")
  (setq evil-want-keybinding nil)
  (global-evil-surround-mode t)
  (evil-mode t)

  (let ((map evil-inner-text-objects-map))
    (define-key map "i" 'evil-indent-plus-i-indent)
    (define-key map "I" 'evil-indent-plus-i-indent-up)
    (define-key map "J" 'evil-indent-plus-i-indent-up-down))

  (let ((map evil-outer-text-objects-map))
    (define-key map "i" 'evil-indent-plus-a-indent)
    (define-key map "I" 'evil-indent-plus-a-indent-up)
    (define-key map "J" 'evil-indent-plus-a-indent-up-down))

  (let ((map evil-normal-state-map))
    (define-key map "j" 'evil-next-visual-line)
    (define-key map "k" 'evil-previous-visual-line)
    (define-key map "=" 'evil-indent-line)
    (define-key map "<" 'evil-shift-left-line)
    (define-key map ">" 'evil-shift-right-line)

    ;; Unset vim reundo keybinding and use C-? to reundo.
    (define-key map (kbd "C-r") nil)

    ;; Unset vim keybinding to start a macro.
    (define-key map "q" nil))

  (let ((map evil-visual-state-map))
    (define-key map "=" 'evil-visual-indent)
    (define-key map "<" 'evil-visual-shift-left)
    (define-key map ">" 'evil-visual-shift-right))

  (let ((map evil-motion-state-map))
    (define-key map "!" 'shell-command)
    (define-key map ":" 'goto-line)

    ;; Remove evil search and some motions.
    (define-key map "}" nil)
    (define-key map "{" nil)
    (define-key map "/" nil)
    (define-key map "*" nil))

  ;; Sets evil keybindings by mode.
  (dolist (mode evil-collection-mode-list)
    (with-eval-after-load mode
      (evil-collection-init (list mode))))

  ;; Sets evil with `magit'.
  (with-eval-after-load 'magit
    (require 'evil-magit)))

;;;###autoload
(defer-loading
  :event emacs-startup-hook
  :function evil-initialize)

;;; evil.el ends here
