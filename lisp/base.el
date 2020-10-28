;;; lisp/base.el --- base configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: base

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
(advice emacs-startup-hook
        setup-frame-title)

;;;###autoload
(defun setup-frame-title ()
  "Setup frame title configurations."
  (setq frame-title-format (list :eval '(embla-frame-title))))

(defun embla-frame-title ()
  "Return frame title eval in `setup-frame-title'."
  (let* ((buffer (buffer-name))
         (path (when (buffer-file-name)
                (expand-file-name (buffer-file-name)))))
    (unless path
      (setq buffer (downcase
                    (string-trim (string-trim (buffer-name)) "*" "*")))
      (when (string-match "\\minibuf-" buffer)
        (setq buffer (symbol-name (ivy-state-caller ivy-last)))))
    (concat "Embla : " (if path path buffer))))