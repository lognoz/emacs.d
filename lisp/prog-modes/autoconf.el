;;; lisp/languages/autoconf.el --- auto configurations  -*- lexical-binding: t -*-

;; Copyright (c) Marc-Antoine Loignon
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: auto configuration

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
(defvar embla-auto-install-languages
  '((:mode puppet-mode  :package puppet-mode  :syntax ("_"))
    (:mode yaml-mode    :package yaml-mode    :syntax ("-" "_")  :pattern ("\\.yml\\'"))
    (:mode scss-mode    :package scss-mode    :syntax ("-")      :pattern ("\\.scss\\'"))
    (:mode latex-mode   :syntax ("\\"))))

;;;###autoload
(dolist (parameters embla-auto-install-languages)
  (let* ((package (plist-get parameters :package))
         (mode (plist-get parameters :mode))
         (syntax (plist-get parameters :syntax))
         (pattern (plist-get parameters :pattern))
         (hook (intern (format "%s-hook" mode)))
         (component (intern (format "embla-%s" hook)))
         (docstring (format "Setup %s component configurations." mode)))
    (when package
      (require-package package))
    (when pattern
      (bind-patterns mode pattern))
    (when syntax
      (setq syntax (format "%s" syntax))
      (setq syntax (string-remove-suffix ")" syntax))
      (setq syntax (string-remove-prefix "(" syntax))
      (eval
        `(define-component ,component (,hook)
          ,docstring
          (define-syntax-entries ,syntax))))))

;;; auto-install.el ends here
