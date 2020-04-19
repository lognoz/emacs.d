;;; config.el --- Company Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: autocomplete

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

(add-hook 'pre-command-hook 'company-completion-hook)
(add-hook 'find-file-hook 'company-completion-hook)

(defun company-completion-hook ()
  (company--setup-keybindings)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-limit 14
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  (global-company-mode 1))

(defun company--setup-keybindings ()
  (with-eval-after-load 'company
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)))
