;;; config.el --- Org Mode File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: org

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
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;;###autoload
(add-hook 'org-mode-hook 'org-mode-component-hook)

;;;###autoload
(defun org-mode-component-hook ()
  (org-bullets-mode)
  (toc-org-mode)
  (org-init-org)

  (setq org-startup-indented t
          org-clock-idle-time 5
          org-bullets-bullet-list '("› ")
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-agenda-block-separator ""
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t
          org-catch-invisible-edits 'show-and-error
          org-cycle-separator-lines 0))

(defun org-init-org ()
  (org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)
      (http . t)
      (emacs-lisp . t)
      (python . t))))
