;;; lisp/editor/company.el --- company configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: company

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

(require-package 'company
                 'company-prescient)


;;;###autoload
(defun company-initialize ()
  "Sets company configurations."
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.25)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 14)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-other-buffers t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  (global-company-mode t)
  (company-prescient-mode t))

;;;###autoload
(with-eval-after-load 'company
  (let ((map company-active-map))
    (define-key map (kbd "<tab>") 'company-complete-selection)
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)))

;;;###autoload
(defer-loading
  :event pre-command-hook
  :function company-initialize)

;;; company.el ends here
