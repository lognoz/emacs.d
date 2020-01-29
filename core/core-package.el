;;; core-package.el --- Core Package File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: packages

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

(require 'package)

(defvar embla-package-initialized nil
  "Package content has been initialized.")

(defvar embla-component-packages nil
  "List of packages required by component.")

(defvar embla-mode-alist
  ;; Language        Extension              Word syntax       Mode
  '(("dired"         nil                    nil               dired-mode)
    ("ibuffer"       nil                    nil               ibuffer-mode)
    ("php"           "\\.php\\'"            '("_" "$")        php-mode)
    ("web"           "\\.html.php\\'"       '("-" "_" "$")    web-mode)
    ("javascript"    "\\.js\\'"             '("-" "_")        js2-mode)
    ("scss"          "\\.scss\\'"           '("-" "_")        scss-mode)
    ("yaml"          "\\.yml\\'"            '("-" "_")        yaml-mode)
    ("puppet"        "\\.pp\\'"             '("-" "_")        puppet-mode)
    ("pkgbuild"      "PKGBUILD\\'"          '("-" "_")        pkgbuild-mode)
    ("dockerfile"    "Dockerfile\\'"        '("-" "_")        dockerfile-mode)
    ("json"          "\\.json\\'"           '("-" "_")        json-mode)
    ("emacs"         "\\.el\\'"             '("-" "_")        emacs-lisp-mode)
    ("org"           "\\.org\\'"            '("-" "_")        org-mode)
    ("latex"         "\\.tex\\'"            '("\\")           latex-mode)
    ("editorconfig"  "\\.editorconfig\\'"   '("-" "_")        editorconfig-conf-mode)
    ("python"        "\\.py[iw]?\\'"        '("_")            python-mode)))

;;; Internal core functions.

(defun package--initialize ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (setq embla-package-initialized t))

;;; External core functions.

(defun require-package (package)
  (interactive)
  ;; Add archive and initialize package.
  (unless embla-package-initialized
    (package--initialize))
  ;; Install package if not installed.
  (when (not (package-installed-p package))
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package))
  ;; Push value into `embla-component-packages' variable.
  (add-to-list 'embla-component-packages (format "%s" package)))

(provide 'core-package)
