;;; lisp/progmodes/web-mode.el --- Extensions to web mode -*- lexical-binding: t -*-

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
(embla-elpa-package 'web-mode
  (add-hook 'web-mode-hook #'embla-set-web-mode))

;;;###autoload
(embla-elpa-package 'scss-mode
  (add-hook 'scss-mode-hook #'embla-set-scss-mode))

(embla-elpa-package 'emmet-mode
  (define-key web-mode-map (kbd "C-<return>") #'emmet-expand-line)
  (define-key scss-mode-map (kbd "C-<return>") #'emmet-expand-line))

(embla-elpa-package 'smartparens)
(embla-elpa-package 'paredit)
(embla-elpa-package 'auto-rename-tag)
(embla-elpa-package 'company)
(embla-elpa-package 'company-web)

;;;###autoload
(setq auto-mode-alist
      (append '(("\\.html\\'" . web-mode)
                ("\\.html.php\\'" . web-mode)
                ("\\.scss\\'" . scss-mode))
              auto-mode-alist))

;;;###autoload
(defun embla-set-scss-mode ()
  "Setup scss component configurations."
  (modify-syntax-entry ?- "w"))

;;;###autoload
(defun embla-set-web-mode ()
  "Setup web component configurations."
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?- "w")
  (auto-rename-tag-mode t)
  (smartparens-mode t)
  (lsp)

  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
        (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  (set (make-local-variable 'company-backends)
       '(company-css
         company-capf
         company-web-html
         company-yasnippet
         company-files)))

;;; web-mode.el ends here
