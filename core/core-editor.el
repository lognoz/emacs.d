;;; core-editor.el --- Core Editor Initialization File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: editor config

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

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Hide the cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Select help window
(setq help-window-select t)

;; Use system trash for file deletion
(setq delete-by-moving-to-trash t)

;; Highlight current line
(global-hl-line-mode t)

;; No blink
(blink-cursor-mode 0)

;; When emacs asks for "yes" or "no", let "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Highlight matching parenthesis
(show-paren-mode t)

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Auto close insertion
(electric-pair-mode 1)

;; Define tab width
(setq-default tab-width 3)

;; Define line spacing
(setq-default line-spacing 2)

;; Define Emacs frame title format
(setq frame-title-format '("%b - Emacs"))

;; Change default font.
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; Change face for matching elements.
(face-spec-set 'lazy-highlight
  '((t :foreground "#C678DD"
       :background "#20242B"
       :weight bold
       :underline nil)))

(face-spec-set 'match
  '((t :foreground "#C678DD"
       :background "#20242B"
       :weight bold
       :underline nil)))

(face-spec-set 'highlight
  '((t :background "#20242B"
       :foreground nil)))

(dolist (selector '(vr/match-0 vr/match-1))
  (face-spec-set selector
    '((t :foreground "#C678DD"
        :background "#20242B"
        :weight bold
        :underline nil))))

;; Use UTF-8 as the default coding system
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Enable somes usefull functions
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'overwrite-mode 'disabled t)

;; Enable winner mode for window management
(winner-mode 1)

(provide 'core-editor)
