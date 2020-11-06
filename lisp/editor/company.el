;;; lisp/editor/company.el --- company configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: company

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
(eval-before-init
  (require-package 'company)
  (require-package 'company-box))

;;;###autoload
(define-component embla-company (pre-command-hook)
  "Setup company component configurations."
  (setup-company)
  (global-company-mode t))

;;;###autoload
(define-component embla-on-company (company-mode-hook)
  "Setup company hook component configurations."
  (setup-company-box)
  (after-booting-company))

;;;###autoload
(defun setup-company ()
  "Setup company configurations."
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t))

;;;###autoload
(defun after-booting-company ()
  "Setup company keybindings after it's started."
  (bind-keys company-active-map
    ("<tab>" . company-confirm-selection)
    ("C-n"   . company-select-next)
    ("C-p"   . company-select-previous)))

(defun setup-company-box ()
  "Setup company box configurations."
  (when (display-graphic-p)
    (company-box-mode t)
    (setq company-box-enable-icon nil)
    (setq company-box-backends-colors nil)
    (advice-add #'company-box--update-scrollbar :around #'company-box-hide-scrollbar)))

(defun company-box-hide-scrollbar (orig-fn &rest args)
  "Hide scrollbar when `company-box-mode' is actived."
  (cl-letf (((symbol-function #'display-buffer-in-side-window)
             (symbol-function #'ignore)))
    (apply orig-fn args)))

(defun company-confirm-selection (&optional arg)
  "Confirm the selected candidate on company mode.
If there's no selection, this function will choose the first one."
  (interactive "p")
  (unless company-selection
    (company-select-next))
  (company-complete-selection))

;;; company.el ends here
