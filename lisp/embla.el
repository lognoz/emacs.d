;;; lisp/embla.el --- Extensions to embla -*- lexical-binding: t -*-

;; Copyright (c) Marc-Antoine Loignon <developer@lognoz.org>

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

;;; Commentary:

;;; Code:

;;;###autoload
(embla-autoload "embla" emacs-startup-hook)


;;;; --- Motions

(define-key embla-mode-map (kbd "M-DEL") #'cycle-spacing)


;;;; --- Mark rings

(embla-site-lisp-package "https://github.com/liuyinz/binky-mode"
  (binky-mode)
  (binky-margin-mode)

  (face-spec-set 'binky-preview-header
    '((t :underline nil :weight bold))
    'face-defface-spec)

  (let ((map evil-normal-state-map))
    (define-key map (kbd "m") #'binky-add)
    (define-key map (kbd ",") #'binky-binky)))

;;;###autoload (define-key embla-mode-map (kbd "C-x ,") #'embla-pop-local-mark-ring)
;;;###autoload
(defun embla-pop-local-mark-ring ()
  "Jump to precedent mark ring."
  (interactive)
  (set-mark-command t))


;;;; --- Narrow configurations

(put 'narrow-to-region 'disabled nil)

;;;###autoload (define-key embla-mode-map (kbd "C-x n d") #'embla-switch-function-narrowing)
;;;###autoload
(defun embla-switch-function-narrowing ()
  "Switch `narrow-to-defun' or `org-narrow-to-subtree'."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (if (eq major-mode 'org-mode)
        (org-narrow-to-subtree)
      (narrow-to-defun))))


;;;; --- Expand configurations

(embla-elpa-package 'expand-region
  (autoload 'er/expand-region "expand-region")
  (autoload 'er/mark-word "expand-region")
  (autoload 'er/mark-inside-quotes "expand-region")
  (autoload 'er/mark-inside-pairs "expand-region")
  (autoload 'er/mark-comment "expand-region"))

(let ((map evil-motion-state-map))
  (define-key map (kbd "'") #'er/expand-region)
  (define-key map (kbd "1") #'er/mark-word)
  (define-key map (kbd "2") #'er/mark-inside-quotes)
  (define-key map (kbd "3") #'er/mark-inside-pairs)
  (define-key map (kbd "9") #'er/mark-comment))

;;; embla.el ends here
