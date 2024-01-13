;;; lisp/progmodes/prog-mode.el --- Extensions to prog mode -*- lexical-binding: t -*-

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

;;; Code:

(embla-elpa-package 'linum-relative)

(embla-elpa-package 'lsp-mode)

;;;###autoload
(defun embla-lsp ()
  "Setup lsp component configurations."
  (interactive)
  (let ((system-type 'gnu/linux))
    (lsp)))

;;;###autoload
;; (embla-builtin-package 'prog-mode
;;   (add-hook 'prog-mode-hook #'embla-set-prog-mode))

;;;###autoload
(defun embla-set-prog-mode ()
  "Setup prog component configurations."
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-mode t)
  (display-line-numbers-mode t))

;;; prog-mode.el ends here
