;;; component-autocomplete.el - Autocomplete Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: autocomplete company snippet

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

(defun component-autocomplete/install ()
  "Install compagny requirements."
  (add-package (company yasnippet))
  (add-hook 'after-init-hook 'global-company-mode))

(defun component-autocomplete/setup-configuration ()
  "Set company mode configurations."
  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)))

(component-autocomplete/install)
(component-autocomplete/setup-configuration)

(require 'yasnippet)
(require 'company)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(with-eval-after-load 'company
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))


(provide 'component-autocomplete)
