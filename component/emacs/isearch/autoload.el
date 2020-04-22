;;; autoload.el --- Isearch Autoload Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: isearch

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
(defun occur-project (keyword)
  (interactive "sGrep for: ")
  (let ((directory (projectile-project-root)))
    (if directory
      (noccur-project keyword 0 directory)
      (noccur-project keyword))))

;;;###autoload
(defadvice isearch-exit (after my-goto-match-beginning activate)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
