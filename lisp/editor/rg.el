;;; lisp/editor/rg.el --- rg configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: rg

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
(eval-before-init
  (require-package 'rg)
  (require-package 'find-file-in-project))

;;;###autoload
(bind-keys embla-mode-map
  ("M-s g" . execute-rg))

;;;###autoload
(autoload 'rg-run "rg")

;;;###autoload
(defun execute-rg (start end)
  "Execute rg command with supplied START and END."
  (interactive "r")
  (let* ((root (ffip-project-root))
         (default
           (if (use-region-p)
               (buffer-substring-no-properties start end)
            (thing-at-point 'word 'no-properties)))
         (prompt
           (concat "Regex search for"
                   (when default
                      (concat " (default " default ")"))
                   ": ")))
    (let ((pattern (read-string prompt)))
      (when (equal pattern "")
        (setq pattern default))
      (rg-run pattern "all" root))))

;;; rg.el ends here
