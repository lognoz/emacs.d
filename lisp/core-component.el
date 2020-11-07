;;; lisp/core-component.el --- core component -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: core component

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

(defvar embla-components nil
  "The list of components defined.")

(defmacro define-component (name events &optional docstring &rest body)
  "Define NAME as a component function.
The definition is (lambda (&rest _) [DOCSTRING] BODY...).

This function can defer loading on EVENTS. If none is given it
will use `emacs-startup-hook'."
  (unless (listp events)
    (error "Malformed arglist: %s" events))
  (eval
   `(defun ,name (&rest _)
      ,docstring
      ,@body))
  `(let ((events ',events))
    (when (null events)
      (setq events '(emacs-startup-hook)))
    (push '(,name . (,events)) embla-components)
    (mapcar (lambda (event)
              (if (functionp event)
                  (advice-add event :before ',name)
                (add-hook event ',name)))
            events)))


(provide 'core-component)

;;; core-component.el ends here
