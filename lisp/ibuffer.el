;;; lisp/ibuffer.el --- ibuffer configurations -*- lexical-binding: t; -*-

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

;;;###autoload
(eval-before-init
  (require-package 'ibuffer-projectile))

;;;###autoload
(define-component ibuffer-dired (ibuffer-mode-hook)
  "Setup ibuffer component configurations."
  (ibuffer-do-sort-by-alphabetic)
  (ibuffer-projectile-set-filter-groups)
  (setup-ibuffer))

;;;###autoload
(bind-keys embla-mode-map
  ("C-x C-b" . ibuffer))

;;;###autoload
(defun setup-ibuffer ()
  "Setup ibuffer configurations."
  (evil-collection-define-key 'normal 'ibuffer-mode-map
    (kbd "RET") 'ibuffer-visit-buffer)

  ;; Set size format for more human-friendly readability.
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
      ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
      ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
      (t (format "%8d" (buffer-size)))))

  ;; Change ibuffer format.
  (setq ibuffer-formats
    '((mark modified read-only " "
        (name 50 50 :left :nil) " "
        (size-h 9 -1 :right) " "
        (mode 16 16 :left :elide) " "
        filename-and-process))))

;;; ibuffer.el ends here
