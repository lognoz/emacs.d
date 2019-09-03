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
  (core-base/setup-ido)
  (core-base/setup-theme))

(defun core-base/setup-theme ()
  "Define Emacs theme."
  (install (atom-one-dark-theme))
  (load-theme 'atom-one-dark t))

(defun core-base/setup-ido ()
  "Setup ido mode."
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-separator "  "
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10))

(defun core-base/create-elpa-repository ()
  "Create an ELPA repository."
  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(provide 'core-base)
