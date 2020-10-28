;;; lisp/emacs/help.el --- help configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: help

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
  ("C-h M" . woman)
  ("C-h l" . language-man)
  ("C-h s" . stackoverflow-search))

;;;###autoload
(defun language-man ()
  "Launch manual by `major-mode'."
  (interactive)
  (if (derived-mode-p 'emacs-lisp-mode)
      (find-function-read)
    (let* ((mode (symbol-name major-mode))
           (manual (intern (concat mode "-manual"))))
      (if (fboundp manual)
          (funcall-interactively manual)
        (message "No manual found for '%s'" mode)))))

;;;###autoload
(defun stackoverflow-search ()
  "Browse stackoverflow search page with a keywords."
  (interactive)
  (let ((keywords (read-string "Keywords: "))
        (page "https://stackoverflow.com/search?q=%s"))
    (browse-url (format page keywords))
    (message "Browse '%s' in stackoverflow" keywords)))


;;; help.el ends here
