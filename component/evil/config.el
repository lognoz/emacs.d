;;; config.el --- Evil Component File

;; Copyright (c) Marc-Antoine Loignon

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
  ;; Remap toggle key
  (setq evil-toggle-key "C-`"
        evil-want-keybinding nil)

  ;; Require evil to changes some motion and keybindings.
  (require 'evil)

  ;; Enable Evil
  (global-evil-surround-mode 1)
  (evil-mode 1)

  ;; Remove the vim way to execute a macro.
  (define-key evil-normal-state-map "q" nil)
  (define-key evil-motion-state-map "}" nil)
  (define-key evil-motion-state-map "{" nil)

  ;; Remove the vim way to search something.
  (define-key evil-motion-state-map "/" nil)
  (define-key evil-motion-state-map "*" nil)

  ;; Remove the vim way to reundo to keep native Emacs backward
  ;; search. Use C-? to reundo instead.
  (define-key evil-normal-state-map "\C-r" nil)

  ;; Add useful motions.
  (define-key evil-normal-state-map (kbd "RET") 'newline)

  ;; Use shell-command function for '!' evil motions.
  (define-key evil-motion-state-map "!" 'shell-command)

  ;; Use goto-line function for ':' evil motions.
  (define-key evil-motion-state-map ":" 'goto-line)

  ;; Normal state
  (define-key evil-normal-state-map "=" 'evil-indent-line)
  (define-key evil-normal-state-map "<" 'evil-shift-left-line)
  (define-key evil-normal-state-map ">" 'evil-shift-right-line)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;;;; Visual state
  (define-key evil-visual-state-map "=" 'visual-indent)
  (define-key evil-visual-state-map "<" 'visual-shift-left)
  (define-key evil-visual-state-map ">" 'visual-shift-right)
  (define-key evil-visual-state-map [remap evil-repeat] 'evil-repeat)

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

(defun evil/init-evil-smartparens ()
  (require 'smartparens-config)
  (smartparens-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(defun evil/init-evil-magit ()
  (with-eval-after-load 'magit
    (require 'evil-magit)))

(defun evil/init-evil-collection ()
  (with-eval-after-load 'evil
    (evil-collection-init)))

(defun evil/init-evil-indent-plus ()
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))
