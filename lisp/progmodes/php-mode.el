;;; lisp/progmodes/php-mode.el --- Extensions to php mode -*- lexical-binding: t -*-

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

;;; Code:

;;;###autoload
(embla-elpa-package 'php-mode
  (add-hook 'php-mode-hook #'embla-set-php-mode)
  (add-hook 'php-mode-hook #'embla-set-company-php))

(embla-elpa-package 'ac-php
  (ac-php-core-eldoc-setup)
  (define-key php-mode-map (kbd "M-]") #'ac-php-find-symbol-at-point)
  (define-key php-mode-map (kbd "M-[") #'ac-php-location-stack-back))

(embla-elpa-package 'company)
(embla-elpa-package 'company-php)
(embla-elpa-package 'company-phpactor)
(embla-elpa-package 'phpactor)

;;;###autoload
(defun embla-set-php-mode ()
  "Setup php component configurations."
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?$ "w")
  (setq php-phpdoc-font-lock-doc-comments nil)
  (face-remap-add-relative 'font-lock-doc-face 'font-lock-comment-face))

;;;###autoload
(defun embla-set-company-php ()
  "Setup company configurations."
  (set (make-local-variable 'company-backends)
       '(company-ac-php-backend
         company-phpactor
         company-files)))

;;; php-mode.el ends here
