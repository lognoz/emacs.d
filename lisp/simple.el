;;; lisp/simple.el --- basic editing commands configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: simple

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
  ("M-DEL" . cycle-spacing)
  ("s-y"   . copy-whole-bufer)
  ("C-x h" . mark-whole-buffer)
  ("C-x ," . pop-local-mark-ring))

;;;###autoload
(defun copy-whole-buffer ()
  "Copy the entire buffer to the kill-ring."
  (interactive)
  (copy-region-as-kill 1 (point-max))
  (message "The entire buffer is copied to the kill-ring."))

;;;###autoload
(defun pop-local-mark-ring ()
  "Jump to precedent mark ring."
  (interactive)
  (set-mark-command t))

;;; simple.el ends here
