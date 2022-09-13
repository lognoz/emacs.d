;;; lisp/editor/snippet.el --- Extensions to snippet -*- lexical-binding: t -*-

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
(embla-autoload "editor/snippet" text-mode-hook prog-mode-hook conf-mode-hook)

(defconst embla-snippet-directory (expand-file-name "snippet" embla-private-directory)
  "The directory of snippet files.")

(embla-elpa-package 'yasnippet
  (yas-global-mode t)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (when (file-directory-p embla-snippet-directory)
    (setq yas-snippet-dirs '(embla-snippet-directory))))

;;;###autoload
(defun embla-company-yasnippet-backend (backends)
  "Append yasnippet to company BACKENDS if it's not part of it."
  (if (and (listp backends) (member 'company-yasnippet backends))
      backends
    (append (if (consp backends) backends (list backends))
            '(:with company-yasnippet))))

;;; snippet.el ends here
