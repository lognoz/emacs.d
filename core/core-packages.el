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

(defvar embla-component-packages nil
  "List of packages required by component.")

;;; External macro functions.

(cl-defmacro packadd! (name &rest body &key config build-in)
  ;; Install package if not installed.
  (unless (package-installed-p name) (package-install name))
  ;; Handle :config and execute it.
  (when config (eval config))
  ;; Push value into `embla-component-packages' variable.
  (macroexp-progn
    `((add-to-list 'embla-component-packages (format "%S" ',name)))))

;;; External core functions.

(defun core-packages/embla-startup-hook ()
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))
