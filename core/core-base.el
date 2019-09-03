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
  ; To remove
  (defvar atom-one-dark-colors-alist
          `(("atom-one-dark-accent"   . "#528BFF")
            ("atom-one-dark-fg"       . "#ABB2BF")
            ("atom-one-dark-bg"       . "#000000")
            ("atom-one-dark-bg-1"     . "#121417")
            ("atom-one-dark-bg-hl"    . "#2C323C")
            ("atom-one-dark-gutter"   . "#4B5363")
            ("atom-one-dark-mono-1"   . "#ABB2BF")
            ("atom-one-dark-mono-2"   . "#828997")
            ("atom-one-dark-mono-3"   . "#5C6370")
            ("atom-one-dark-cyan"     . "#56B6C2")
            ("atom-one-dark-blue"     . "#61AFEF")
            ("atom-one-dark-purple"   . "#C678DD")
            ("atom-one-dark-green"    . "#98C379")
            ("atom-one-dark-red-1"    . "#E06C75")
            ("atom-one-dark-red-2"    . "#BE5046")
            ("atom-one-dark-orange-1" . "#D19A66")
            ("atom-one-dark-orange-2" . "#E5C07B")
            ("atom-one-dark-gray"     . "#3E4451")
            ("atom-one-dark-silver"   . "#9DA5B4")
            ("atom-one-dark-black"    . "#000000")
            ("atom-one-dark-border"   . "#000000")))
  (load-theme 'atom-one-dark t))

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
