;;; lisp/interface.el --- User interface -*- lexical-binding: t -*-

;; Copyright (C)  Marc-Antoine Loignon <developer@lognoz.org>

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

;;; Code:

(embla-elpa-package 'atom-one-dark-theme)

(embla-elpa-package 'which-key
  (which-key-mode t)
  (which-key-setup-minibuffer))

(embla-elpa-package 'orglink
  (global-orglink-mode t))


;;; --- Interface utilities

(defcustom embla-theme 'atom-one-dark
  "The symbol of a theme to be loaded at Emacs startup.
If you want to change the default theme you need to redefine this
variable. See example below:
(setq embla-theme 'my-custom-theme)"
  :group 'embla
  :type 'symbol)

(defcustom embla-font
  (cond
    ((find-font (font-spec :name  "Hasklug Nerd Font Mono")) "Hasklug Nerd Font Mono")
    ((find-font (font-spec :name  "Droid Sans Mono")) "Droid Sans Mono"))
  "The default font to use in Embla.
To change the Emacs font, you need to redefine this variable.
See example below:
(setq embla-font (font-spec :family \"Source Code Pro\" :height 100)"
  :group 'embla
  :type 'font-spec)

;;;###autoload
(defun embla-load-theme ()
  "Load theme by its definition in `embla-theme' variable."
  (when embla-theme
    (load-theme embla-theme t)))

;;;###autoload
(defun embla-set-font ()
  "Set default font by its definition in `embla-font' variable."
  (when embla-font
    (set-frame-font embla-font 'keep-size t)))

;;;###autoload
(defun embla-set-frame-title ()
  "Set frame title configurations."
  (setq frame-title-format (list :eval '(embla-frame-title))))

(defun embla-frame-title ()
  "Return frame title eval in `embla-set-frame-title'."
  (let* ((buffer (buffer-name))
         (path (when (buffer-file-name)
                (expand-file-name (buffer-file-name)))))
    (unless path
      (setq buffer (downcase
                    (string-trim (string-trim (buffer-name)) "*" "*")))
      (if (derived-mode-p 'sr-mode 'dired-mode)
          (setq path (expand-file-name default-directory))
        (when (string-match "\\minibuf-" buffer)
          (setq buffer (symbol-name (ivy-state-caller ivy-last))))))
    (concat "Embla : " (if path path buffer))))

;;;###autoload
(embla-builtin-package 'custom
  (embla-load-theme)
  (embla-set-font)
  (embla-set-frame-title))


;;; --- Mouse and smooth scroll

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

(setq scroll-step 1)
(setq scroll-margin 0)
(setq scroll-conservatively 100000)


;;; Line configuration

;; Change the line spacing for a better visibility.
(setq-default line-spacing 4)

;; Highlight current line.
(global-hl-line-mode t)


;;; Cursor configuration

;; The blinking cursor is distracting.
(blink-cursor-mode -1)

;; Hide the cursor in inactive windows.
(setq cursor-in-non-selected-windows nil)

;; Don't blink the paren matching.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters.
(setq x-stretch-cursor nil)


;;; Disable splash screen and UI elements

;;;###autoload
(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;;; Shows vim-style tildes on the left fringe

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)

;;; interface.el ends here
