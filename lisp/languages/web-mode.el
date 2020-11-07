;;; lisp/languages/web-mode.el --- web configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: web

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
  (require-package 'company-web)
  (require-package 'web-mode)
  (require-package 'emmet-mode))

;;;###autoload
(define-component embla-web-mode (web-mode-hook)
  "Setup web component configurations."
  (setup-web))

;;;###autoload
(bind-patterns 'web-mode '("\\.html\\'" "\\.html.php\\'"))

;;;###autoload
(defun setup-web ()
  "Setup web configurations."
  (define-syntax-entries "_" "-")
  (set (make-local-variable 'company-backends)
       '(company-css
         company-web-html
         company-yasnippet
         company-files)))

;;; web-mode.el ends here
