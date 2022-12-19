;;; lisp/isearch.el --- Extensions to isearch -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

;;;###autoload
(embla-autoload "isearch" emacs-startup-hook)


;;;; --- Isearch configurations

(embla-builtin-package 'isearch
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

  (define-key isearch-mode-map (kbd "M-s r") #'isearch-query-replace)

  (defadvice isearch-exit (after my-goto-match-beginning activate)
    (when (and isearch-forward isearch-other-end)
      (goto-char isearch-other-end))))

(embla-elpa-package 'visual-regexp
  (setq vr/auto-show-help nil)
  (let ((map embla-mode-map))
    (define-key map (kbd "M-s r") #'vr/query-replace)
    (define-key map (kbd "M-s M-r") #'vr/replace)))

;;; isearch.el ends here
