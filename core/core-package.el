;;; core-package.el - Core Package File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(defun core-package/setup-theme ()
  "Define Emacs theme."
  (add-package (atom-one-dark-theme))
  (load-theme 'atom-one-dark t))

(defun core-package/setup-ido ()
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

(provide 'core-package)
