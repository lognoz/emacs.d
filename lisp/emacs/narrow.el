;;; lisp/emacs/narrow.el --- narrow configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: narrow

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
(bind-keys embla-mode-map
  ("C-x n d" . switch-function-narrowing))

;;;###autoload
(put 'narrow-to-region 'disabled nil)

;;;###autoload
(defun switch-function-narrowing ()
  "Switch `narrow-to-defun' or `org-narrow-to-subtree'."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (if (eq major-mode 'org-mode)
        (org-narrow-to-subtree)
      (narrow-to-defun))))

;;; narrow.el ends here
