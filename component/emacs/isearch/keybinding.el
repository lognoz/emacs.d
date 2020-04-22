;;; keybinding.el --- Isearch Keybinding Component File

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

(define-keybinding
  :mode 'embla-mode-map
  :define
    (kbd "M-s r") 'vr/query-replace
    (kbd "M-s M-r") 'vr/replace
    (kbd "M-s M-o") 'occur-project)

(define-keybinding
  :mode 'isearch-mode-map
  :define
    (kbd "M-s r") 'isearch-query-replace)
