;;; core-base.el - Core Base File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(require 'package)

(defmacro install (package-list)
  "Install packages if not installed."
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun core-base/init ()
  "Perform core base initialization."
  (core-base/create-elpa-repository)
  (core-base/setup-ido))

(defun core-base/setup-ido ()
  "Setup ido mode and use vertical aligment."
  (install (ido-vertical-mode))
  (require 'ido-vertical-mode)
  (setq ido-vertical-show-count t)
  (ido-mode 1)
  (ido-vertical-mode 1))

(defun core-base/create-elpa-repository ()
  "Create an ELPA repository."
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(provide 'core-base)
