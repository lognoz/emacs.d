;;; lisp/autoloads/snippet.el --- snippet autoloads -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: snippet

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
  (require-package 'yasnippet))

;;;###autoload
(define-component embla-snippet (company-mode-hook)
  "Setup snippet component configurations."
  (setup-snippet))

(define-component embla-snippet-mode (text-mode-hook prog-mode-hook conf-mode-hook)
  "Load up yasnippet mode."
  (yas-global-mode t))

(defconst embla-snippet-directory (expand-file-name "snippet" embla-private-directory)
  "The directory of snippet files.")

;;;###autoload
(defun setup-snippet ()
  "Setup snippet configurations."
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (setq yas-snippet-dirs '(embla-snippet-directory))
  (setq company-backends (mapcar #'company-yasnippet-backend company-backends)))

;;;###autoload
(defun company-yasnippet-backend (backends)
  "Append yasnippet to company BACKENDS if it's not part of it."
  (if (and (listp backends) (member 'company-yasnippet backends))
      backends
    (append (if (consp backends) backends (list backends))
            '(:with company-yasnippet))))

;;; snippet.el ends here
