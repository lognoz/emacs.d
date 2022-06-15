;;; lisp/load-up.el --- Load up -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: embla

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

;;; Commentary:

;;; Code:

(defmacro embla-autoload (file &rest events)
  (declare (indent 1))
  `(let* ((file (expand-file-name (concat "lisp/" ,file) user-emacs-directory))
          (events ',events)
          (self-loader (lambda (&rest _)
                         (load file nil 'nomessage))))
     (when (null events)
       (setq events '(emacs-startup-hook)))
     (mapcar (lambda (event)
               (if (functionp event)
                   (advice-add event :before self-loader)
                 (add-hook event self-loader)))
             events)))

(defmacro embla-builtin-package (package &rest body)
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'embla-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro embla-elpa-package (package &rest body)
  (declare (indent 1))
  `(progn
     (when (not (package-installed-p ,package))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'embla-emacs (format "Loading `%s' failed" ,package) :warning))))



;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; load-up.el ends here
