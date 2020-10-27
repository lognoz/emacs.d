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
(boot-packages 'yasnippet)

;;;###autoload
(advice pre-command-hook setup-snippet)

;;;###autoload
(progn (advice text-mode-hook yas-global-mode)
       (advice prog-mode-hook yas-global-mode)
       (advice conf-mode-hook yas-global-mode))

(defconst embla-snippet-directory (expand-file-name "snippet" embla-private-directory)
  "The directory of snippet files.")

;;;###autoload
(defun setup-snippet ()
  "Setup snippet configurations."
  (setq yas-verbosity 1
        yas-wrap-around-region t
        yas-snippet-dirs '(embla-snippet-directory)))

;;;###autoload
(defun company-yasnippet-backend (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;;; snippet.el ends here
