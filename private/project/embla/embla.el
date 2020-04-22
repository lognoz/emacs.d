;;; embla.el --- Embla Project File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: embla project

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

(defvar embla-template-directory (concat embla-project-directory "embla/template/")
  "The directory of template files.")

;;;###autoload
(defvar embla-editing-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\pc" 'embla-create-template)
    (define-key keymap "\C-c\pl" 'embla-reload-init)
    (define-key keymap "\C-c\pd" 'embla-goto-definition)
    keymap))

(defun embla-reload-init ()
  "Reload init configuration."
  (interactive)
  (import-core "installer/installer")
  (load-file embla-core-init)
  (embla-install-program)
  (embla-after-init-hook))

;;;###autoload
(define-minor-mode embla-editing-mode
  "A minor-mode to help to manage embla configuration."
  nil " embla" embla-editing-mode-map)

(provide 'embla)
