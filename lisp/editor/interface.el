;;; lisp/editor/interface.el --- user interface -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: user interface

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
(boot-packages 'atom-one-dark-theme
               'linum-relative
               'which-key
               'orglink)

;;;###autoload
(advice emacs-startup-hook
        load-embla-theme
        set-embla-font
        which-key-mode
        which-key-setup-minibuffer
        global-orglink-mode)

(defcustom embla-theme 'atom-one-dark
  "The symbol of a theme to be loaded at Emacs startup.

If you want to change the default theme you need to redefine this
variable. See example below:

(setq embla-theme 'my-custom-theme)"
  :group 'embla
  :type 'symbol)

(defcustom embla-font nil
  "The default font to use in Embla.

To change the Emacs font, you need to redefine this variable.
See example below:

(setq embla-font (font-spec :family \"Source Code Pro\" :height 100)"
  :group 'embla
  :type 'font-spec)

;;;###autoload
(defun load-embla-theme ()
  "Load theme by its definition in `embla-theme' variable."
  (when embla-theme
    (load-theme embla-theme t)))

;;;###autoload
(defun set-embla-font ()
  "Set default font by its definition in `embla-font' variable."
  (when embla-font
    (set-frame-font embla-font 'keep-size t)))


;;; Mouse and smooth scroll

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil))

(setq scroll-step 1)
(setq scroll-margin 0)
(setq scroll-conservatively 100000)


;;; Line configuration

;; Change the line spacing for a better visibility.
(setq-default line-spacing 2)

;; Highlight current line.
(global-hl-line-mode t)

;; Show hybride line numbers.
(setq linum-relative-backend 'display-line-numbers-mode)
(global-display-line-numbers-mode t)
(linum-relative-mode t)


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
