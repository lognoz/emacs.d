;;; lisp/progmodes/python-mode.el --- Extensions to python mode -*- lexical-binding: t -*-

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
(embla-builtin-package 'python
  (add-hook 'python-mode-hook #'embla-set-python-mode))

(embla-elpa-package 'company)
(embla-elpa-package 'company-jedi)

(embla-eval-on-install
  (jedi:install-server))

;;;###autoload
(defun embla-set-python-mode ()
  "Setup python component configurations."
  (modify-syntax-entry ?_ "w")
  (set (make-local-variable 'company-backends)
       '(company-jedi))
  (define-key python-mode-map (kbd "C-c C-j") #'counsel-imenu))

;;; python-mode.el ends here
