;;; config.el - Evil Component File

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
  (global-unset-key (kbd "C-z"))
  (setq evil-toggle-key "C-`")
  (require 'evil)

  ;; Enable Evil
  (global-evil-leader-mode 1)
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-mode 1)

  (define-key evil-motion-state-map "1" nil)
  (define-key evil-motion-state-map "2" nil)
  (define-key evil-motion-state-map "3" nil)
  (define-key evil-motion-state-map "4" nil)
  (define-key evil-motion-state-map "5" nil)
  (define-key evil-motion-state-map "6" nil)
  (define-key evil-motion-state-map "7" nil)
  (define-key evil-motion-state-map "8" nil)
  (define-key evil-motion-state-map "9" nil)

  ;(define-key evil-normal-state-map "Q" (kbd "@q"))
  ;(define-key evil-visual-state-map "Q" (kbd "@q"))

  ;; Normal state
  (define-key evil-normal-state-map "=" 'evil-indent-line)
  (define-key evil-normal-state-map "<" 'evil-shift-left-line)
  (define-key evil-normal-state-map ">" 'evil-shift-right-line)
  (define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-]") 'helm-etags-select)
  (define-key evil-normal-state-map (kbd "C-j") (concat ":m .+1" (kbd "RET") "=="))
  (define-key evil-normal-state-map (kbd "C-k") (concat ":m .-2" (kbd "RET") "=="))
  (define-key evil-normal-state-map (kbd "C-z") nil)

  ;; Visual state
  (define-key evil-visual-state-map "=" 'visual-indent)
  (define-key evil-visual-state-map "<" 'visual-shift-left)
  (define-key evil-visual-state-map ">" 'visual-shift-right)
  (define-key evil-visual-state-map "." (kbd ":normal ."))
  (define-key evil-visual-state-map (kbd "C-j") (concat ":m '>+1" (kbd "RET") "gv=gv"))
  (define-key evil-visual-state-map (kbd "C-k") (concat ":m '<-2" (kbd "RET") "gv=gv"))

  (evil-define-command evil-record-macro (register)
    :keep-visual t
    :suppress-operator t
    (interactive
     (list (unless (and evil-this-macro defining-kbd-macro)
             (or evil-this-register (evil-read-key)))))
    (cond
     ((eq register ?\C-g)
      (keyboard-quit))
     ((and evil-this-macro defining-kbd-macro)
      (setq evil-macro-buffer nil)
      (condition-case nil
          (end-kbd-macro)
        (error nil))
      (when last-kbd-macro
        (when (member last-kbd-macro '("" []))
          (setq last-kbd-macro nil))
        (evil-set-register evil-this-macro last-kbd-macro))
      (setq evil-this-macro nil))
     ((eq register ?:)
      (evil-command-window-ex))
     ((eq register ?/)
      (evil-command-window-search-forward))
     ((eq register ??)
      (evil-command-window-search-backward))
     ((eq register ?1)
      (call-interactively 'name-last-kbd-macro))
     ((eq register ?2)
      (call-interactively 'insert-kbd-macro))
     ((or (and (>= register ?a) (<= register ?z))
          (and (>= register ?A) (<= register ?Z)))
      (when defining-kbd-macro (end-kbd-macro))
      (setq evil-this-macro register)
      (evil-set-register evil-this-macro nil)
      (start-kbd-macro nil)
      (setq evil-macro-buffer (current-buffer)))
     (t (error "Invalid register"))))

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

(defun evil/init-evil-indent-plus ()
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

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
