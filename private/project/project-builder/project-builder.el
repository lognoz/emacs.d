;;; project-builder.el --- Project Builder File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: project-builder project

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

;;;###autoload
(defvar project-builder-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-c" 'project-builder-execute)
    keymap))

;;; External project functions.

(defun project-builder-execute ()
  (interactive)
  (let* ((default-directory (projectile-project-root)))
    (async-shell-command "make require")))

;;; Define minor mode.

;;;###autoload
(define-minor-mode project-builder-mode
  "A minor-mode to help to code in project builder."
  nil " project-builder" project-builder-mode-map)

(provide 'project-builder)
