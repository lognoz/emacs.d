;;; config.el --- Web Component File

;; Copyright (c) Marc-Antoine Loignon

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

;;; Contextual component variables.

(defvar web-language-loader-hooks '(web-mode-hook)
  "The hook that load Web language.")

;;; Internal component functions.

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.html.php\\'" . web-mode))

(defun web-init-company ()
  (set (make-local-variable 'company-backends)
       '(company-css company-web-html company-yasnippet company-files)))

(defun org-init-emmet ()
  (emmet-mode))
