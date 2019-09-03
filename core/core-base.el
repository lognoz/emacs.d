;;; core-base.el - Core Base File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(require 'package)
(require 'core-ido)
(require 'core-dired)
(require 'core-evil)
(require 'core-version-control)

(defmacro add-package (package-list)
  "Install packages if not installed."
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun core-base/init ()
  "Perform core base initialization."
  (core-base/create-elpa-repository)
  (core-base/setup-theme)
  (core-ido/init)
  (core-evil/init)
  (core-dired/init)
  (core-version-control/init))

(defun core-base/setup-theme ()
  "Setup Emacs theme."
  (add-package (atom-one-dark-theme))
  (load-theme 'atom-one-dark t))

(defun core-base/create-elpa-repository ()
  "Create an ELPA repository."
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(provide 'core-base)
