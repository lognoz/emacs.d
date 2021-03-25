;;; lisp/languages/php-mode.el --- php configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: php

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
  (require-package 'ac-php)
  (require-package 'company)
  (require-package 'company-php)
  (require-package 'company-phpactor)
  (require-package 'php-mode)
  (require-package 'phpactor))

;;;###autoload
(define-component embla-php-mode (php-mode-hook)
  "Setup php component configurations."
  (setup-php)
  (setup-ac-php)
  (setup-company-php))

;;;###autoload
(defun setup-php ()
  "Setup php configurations."
  (define-syntax-entries "_" "$")
  (setq php-phpdoc-font-lock-doc-comments nil)
  (face-remap-add-relative 'font-lock-doc-face 'font-lock-comment-face))

;;;###autoload
(defun setup-ac-php ()
  "Setup eldoc configurations."
  (ac-php-core-eldoc-setup)
  (bind-keys php-mode-map
    ("M-]" . ac-php-find-symbol-at-point)
    ("M-[" . ac-php-location-stack-back)))

;;;###autoload
(defun setup-company-php ()
  "Setup company configurations."
  (set (make-local-variable 'company-backends)
       '(company-ac-php-backend
         company-phpactor
         company-files)))

;;; php-mode.el ends here
