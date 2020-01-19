;;; core-editor.el - Project Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: project

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

(defvar private-project-alist nil
  "List of projects and its minor mode.")

(defun project-enable-minor-mode ()
  (when private-project-alist
    (mapc (lambda (entry)
      (let* ((project (car entry))
             (path (cadr entry))
             (mode (cadr (cdr entry))))
        (when (string-equal (projectile-project-root) path)
          (load (concat embla-project-directory project ".el") nil 'nomessage)
          (funcall mode))))
      private-project-alist)))

(add-hook 'find-file-hook 'project-enable-minor-mode)
(add-hook 'dired-mode-hook 'project-enable-minor-mode)
