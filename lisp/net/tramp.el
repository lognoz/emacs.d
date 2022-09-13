;;; lisp/net/tramp.el --- Extensions to tramp -*- lexical-binding: t -*-

;; Copyright (C)  Marc-Antoine Loignon <developer@lognoz.org>

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/embla
;; Keywords: tramp

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

;;; Code:

(defun su (program)
  "Use root on the current buffer."
  (unless (executable-find program)
    (user-error "Required program \"%s\" not found in your path" program))
  (when buffer-file-name
    (find-alternate-file
     (concat "/" program ":root@localhost:"
             buffer-file-name))))

;;;###autoload
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (su "sudo"))

;;;###autoload
(defun doas ()
  "Use TRAMP to `doas' the current buffer."
  (interactive)
  (su "doas"))

;;; tramp.el ends here
