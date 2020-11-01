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

;;;###autoload
(boot-packages 'evil
               'evil-magit
               'evil-indent-plus
               'evil-smartparens
               'evil-surround
               'evil-collection)

;;;###autoload
(advice emacs-startup-hook
        setup-evil
        setup-evil-collection)

(defvar evil-collection-mode-list
  '(ag
    apropos
    bookmark
    calc
    calendar
    company
    debug
    dired
    help
    ibuffer)
  "The list of `evil-collection' modes to be loaded automatically.")

;;;###autoload
(defun setup-evil ()
  "Setup evil configurations."
  (setq evil-toggle-key "C-`")
  (setq evil-want-keybinding nil)
  (with-eval-after-load 'magit
    (require 'evil-magit))
  (global-evil-surround-mode t)
  (evil-mode t)

  (bind-keys evil-inner-text-objects-map
    ("i" . evil-indent-plus-i-indent)
    ("I" . evil-indent-plus-i-indent-up)
    ("J" . evil-indent-plus-i-indent-up-down))

  (bind-keys evil-outer-text-objects-map
    ("i" . evil-indent-plus-a-indent)
    ("I" . evil-indent-plus-a-indent-up)
    ("J" . evil-indent-plus-a-indent-up-down))

  (bind-keys evil-normal-state-map
    ("j" . evil-next-visual-line)
    ("k" . evil-previous-visual-line)
    ("=" . evil-indent-line)
    ("<" . evil-shift-left-line)
    (">" . evil-shift-right-line))

  (bind-keys evil-visual-state-map
    ("=" . evil-visual-indent)
    ("<" . evil-visual-shift-left)
    (">" . evil-visual-shift-right))

  (bind-keys evil-motion-state-map
    ("!" . shell-command)
    (":" . goto-line))

  ;; Remove the vim way to reundo to keep native Emacs backward
  ;; search. Use C-? to reundo instead.
  (clear-keys evil-normal-state-map "C-r")

  ;; Remove the vim way to start a macro. Use Emacs binding instead.
  (clear-keys evil-normal-state-map "q")

  ;; Remove evil search and some motions.
  (clear-keys evil-motion-state-map "{" "}" "/" "*")

  ;; Remove C-y binding because it's already reserved to paste
  ;; kill ring.
  (clear-keys evil-motion-state-map "C-y")

  ;; Remove the evil way to consult a manual. Use C-h M instead.
  (clear-keys evil-motion-state-map "K"))

;;;###autoload
(defun setup-evil-collection ()
  "Fetch `evil-collection-mode-list' to setup evil
keybindings by mode."
  (dolist (mode evil-collection-mode-list)
    (with-eval-after-load mode
      (evil-collection-init (list mode)))))

;;; evil.el ends here
