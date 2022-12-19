;;; lisp/password.el --- Password utilities -*- lexical-binding: t -*-

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

;;; Commentary:

;; This code provide a minibuffer to copy passwords define under
;; `pass' program.

;;; Code:

(setq epa-pinentry-mode 'loopback)

(defcustom embla-password-directory "~/.password-store/"
  "The directory of gpg password store."
  :group 'embla
  :type 'string)

(defun embla-password--candidates ()
  "Provide candidates to `embla-password' prompt.
Return a list of gpg file located into `embla-password-directory'."
  (let ((list))
    (dolist (f (directory-files embla-password-directory))
      (when (string-match-p ".gpg\\'" f)
        (push (string-trim-right f ".gpg") list)))
    list))

;;;###autoload (define-key embla-mode-map (kbd "M-c") #'embla-password)
;;;###autoload
(defun embla-password ()
  "Provide minibuffer to copy passwords define under `pass' program."
  (interactive)
  (let ((reference (completing-read "Copy password: " (embla-password--candidates) nil t)))
    (with-temp-buffer
      (insert-file-contents (format "%s/%s.gpg" embla-password-directory reference))
      (call-process-shell-command (format "pass -c %s &" reference) nil 0)
      (message (format "Copied %s to clipboard. Will clear in 45 seconds." reference)))))

;;; password.el ends here
