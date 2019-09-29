;;; core-editor.el - Core Editor Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

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

;; Use system trash for file deletion
(setq delete-by-moving-to-trash t)

;; Highlight current line
(global-hl-line-mode t)

;; No blink
(blink-cursor-mode 0)

;; When emacs asks for "yes" or "no", let "y" or "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Highlight matching parenthesis
(show-paren-mode t)

;; Display line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; External core functions.

(defun core-editor/embla-startup-hook ()
  ;; Setup Embla theme
  (packadd! atom-one-dark-theme
            :config (load-theme 'atom-one-dark t))

  (packadd! moody
    :config (setq x-underline-at-descent-line t)
            (moody-replace-mode-line-buffer-identification)
            (moody-replace-vc-mode))

  ;; Setup Editor config to maintain consistent coding styles
  (packadd! editorconfig
            :config (editorconfig-mode 1)))