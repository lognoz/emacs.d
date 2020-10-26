;;; lisp/languages/web.el --- web configurations -*- lexical-binding: t; -*-

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
(boot-packages 'company
               'company-web
               'web-mode
               'emmet-mode)

;;;###autoload
(advice web-mode-hook
        setup-web
        emmet-mode)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.php\\'" . web-mode)))

;;;###autoload
(defun setup-web ()
  "Setup web configurations."
  (set (make-local-variable 'company-backends)
       '(company-css
         company-web-html
         company-yasnippet
         company-files)))

;;; web.el ends here
