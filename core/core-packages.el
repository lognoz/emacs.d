;;; core-packages.el - Core Packages File

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

(defvar embla-package-refresh nil
  "Package content has been refresh.")

(defvar embla-component-packages nil
  "List of packages required by component.")

(defvar embla-languages-alist
  ;; Language        Extension              Word syntax      Built-in    Mode
  '(("php"           "\\.php\\'"            '("_" "$")       nil         php-mode)
    ("web"           "\\.html.php\\'"       '("-" "_" "$")   nil         web-mode)
    ("web"           "\\.html\\'"           '("-" "_")       nil         web-mode)
    ("javascript"    "\\.js\\'"             '("-" "_")       nil         js2-mode)
    ("scss"          "\\.scss\\'"           '("-" "_")       nil         scss-mode)
    ("yaml"          "\\.yml\\'"            '("-" "_")       nil         yaml-mode)
    ("puppet"        "\\.pp\\'"             '("-" "_")       nil         puppet-mode)
    ("pkgbuild"      "PKGBUILD\\'"          '("-" "_")       nil         pkgbuild-mode)
    ("dockerfile"    "Dockerfile\\'"        '("-" "_")       nil         dockerfile-mode)
    ("json"          "\\.json\\'"           '("-" "_")       nil         json-mode)
    ("org"           "\\.org\\'"            '("-" "_")       t           org-mode)
    ("latex"         "\\.tex\\'"            '("\\")          t           latex-mode)
    ("editorconfig"  "\\.editorconfig\\'"   '("-" "_")       t           editorconfig-conf-mode)
    ("python"        "\\.py[iw]?\\'"        '("_")           t           python-mode)))

;;; External macro functions.

(cl-defmacro packadd! (name &rest plist)
  ;; Install package if not installed.
  (when (and (not (package-installed-p name))
             (not (plist-get plist :built-in)))
    (when (not embla-package-refresh)
      (setq embla-package-refresh t)
      (package-refresh-contents))
    (package-install name))
  ;; Handle :config and execute it.
  (let ((current) (config))
    (dolist (statement plist)
      (cond ((eq statement :config) (setq current 'config))
            (t (when (eq current 'config)
              (eval statement))))))
  ;; Push value into `embla-component-packages' variable.
  (macroexp-progn
    `((add-to-list 'embla-component-packages (format "%S" ',name)))))

;;; External core functions.

(defun packages-startup-hook ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize))
