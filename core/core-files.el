;;; core.el - Core Files Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: backup bookmark undo

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

;; Backup files
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-temporary-directory))))

;; Bookmark file
(setq bookmark-default-file (concat user-environment-directory "bookmark"))

;; Undo files
(add-package (undo-tree))
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist
      (list (cons "." (expand-file-name "undo" user-temporary-directory))))

(global-undo-tree-mode 1)

(provide 'core-files)
