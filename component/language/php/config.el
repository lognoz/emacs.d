;;; config.el --- PHP Config File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: php

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

;;; Contextual component variables.

(defvar php-language-loader-hooks '(php-mode-hook)
  "The hook that load PHP language.")

;;; Internal component functions.

(defun php-init-php-mode ()
  ;; Modify PHP syntax entry.
  (define-word-syntax '("_" "$"))
  ;; Reinitialize PHPDoc face to acts like commentary block.
  (setq php-phpdoc-font-lock-doc-comments nil)
  (face-remap-add-relative 'font-lock-doc-face 'font-lock-comment-face))

(defun php-init-ac-php ()
  ;; Enable ElDoc support
  (ac-php-core-eldoc-setup)
  ;; Jump to definition
  (define-key php-mode-map (kbd "M-]")
    'ac-php-find-symbol-at-point)
  ;; Return back
  (define-key php-mode-map (kbd "M-[")
    'ac-php-location-stack-back))

(defun php-init-company ()
  ;; Change company backend.
  (set (make-local-variable 'company-backends)
    '((company-ac-php-backend)
       company-phpactor company-files)))
