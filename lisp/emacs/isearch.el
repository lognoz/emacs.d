;;; lisp/emacs/isearch.el --- isearch configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: isearch

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
  (require-package 'visual-regexp))

;;;###autoload
(define-component isearch-dired ()
  "Setup isearch component configurations."
  (setup-isearch)
  (setup-visual-regexp))

;;;###autoload
(bind-keys embla-mode-map
  ("M-s r"   . vr/query-replace)
  ("M-s M-r" . vr/replace))

;;;###autoload
(defun setup-isearch ()
  "Setup isearch configurations."
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq isearch-yank-on-move 'shif)
  (setq isearch-allow-scroll 'unlimited)

  (bind-keys isearch-mode-map
    ("M-s r" . isearch-query-replace))

  (defadvice isearch-exit (after my-goto-match-beginning activate)
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end))))

;;;###autoload
(defun setup-visual-regexp ()
  "Setup visual-regexp configurations."
  (setq vr/auto-show-help nil))

;;; isearch.el ends here
