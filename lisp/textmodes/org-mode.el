;;; lisp/textmodes/org-mode.el --- Extensions to org mode -*- lexical-binding: t -*-

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

;;;###autoload
(embla-builtin-package 'org
  (add-hook 'org-mode-hook #'embla-set-org-mode))

(embla-elpa-package 'evil-org)
(embla-elpa-package 'org-bullets)
(embla-elpa-package 'ob-async)
(embla-elpa-package 'ob-http)
(embla-elpa-package 'toc-org)
(embla-elpa-package 'org-roam)

;;;###autoload
(defun embla-set-org-mode ()
  "Setup org component configurations."
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

  (require 'org-id)

  (evil-org-mode)

  (org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)
      (http . t)
      (sql . t)
      (emacs-lisp . t)
      (python . t)
      (clojure . t))))

;;; org-mode.el ends here
