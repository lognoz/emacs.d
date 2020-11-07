;;; lisp/tools/version-control.el --- version control configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: version control

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
  (require-package 'magit)
  (require-package 'git-gutter)
  (require-package 'git-gutter+))

;;;###autoload
(define-component embla-version-control ()
  "Setup version control configurations."
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "~")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (setq git-gutter:hide-gutter t)
  (setq git-gutter:ask-p nil)
  (setq git-gutter:hide-gutter t))

;;; version-control.el ends here
