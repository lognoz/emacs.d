;;; component-web.el - Web Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: web

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

(defun component-web/install ()
  "Install web mode requirements."
  (add-package (web-mode emmet-mode))
  (define-mode 'web-mode
    "\\.erb$"
    "\\.html$"
    "\\.php$"
    "\\.rhtml$"))

(defun component-web/setup-indentation ()
  "Set web mode indentation style."
  (setq web-mode-attr-indent-offset 3)
  (setq web-mode-code-indent-offset 3)
  (setq web-mode-css-indent-offset 3)
  (setq web-mode-indent-style 3)
  (setq web-mode-markup-indent-offset 3)
  (setq web-mode-sql-indent-offset 3))

(component-web/install)
(component-web/setup-indentation)

(provide 'component-web)
