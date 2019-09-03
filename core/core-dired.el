;;; core-dired.el - Core Dired File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(defun core-dired/init ()
  "Perform core dired initialization."
  (core-dired/fix-buffer)
  (put 'dired-find-alternate-file 'disabled nil))

(defun core-dired/fix-buffer ()
  "Make dired open in the same window when using RET or ^"
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(provide 'core-dired)
