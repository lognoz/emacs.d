;;; lisp/languages/org-mode.el --- Org mode configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: org mode

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
  (require-package 'org-bullets)
  (require-package 'ob-async)
  (require-package 'ob-http)
  (require-package 'toc-org))

;;;###autoload
(define-component embla-org-mode (org-mode-hook)
  "Setup org component configurations."
  (setup-org)
  (org-bullets-mode)
  (toc-org-mode))

;;;###autoload
(defun setup-org ()
  "Setup org mode configurations."
  (setq org-startup-indented t)
  (setq org-clock-idle-time 5)
  (setq org-bullets-bullet-list '("› "))
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-block-separator "")
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines 0)

  (org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)
      (http . t)
      (emacs-lisp . t)
      (python . t))))

;;; org-mode.el ends here
