;;; lisp/emacs/ibuffer.el --- ibuffer configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: ibuffer

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

(with-eval-after-load 'core-autoloads
  (require-package 'ibuffer-projectile))

;;;###autoload
(defun ibuffer-initialize ()
  "Sets ibuffer configurations."
  (ibuffer-projectile-set-filter-groups)
  (unless (equal ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

;;;###autoload
(defer-loading
  :event ibuffer-mode-hook
  :function ibuffer-initialize)

;;; ibuffer.el ends here
