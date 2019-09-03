;;; core-evil.el - Core Evil File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(defun core-evil/init ()
  "Perform core evil initialization."
  (add-package (evil
                evil-collection
                evil-indent-plus
                evil-leader
                evil-magit
                evil-smartparens
                evil-surround))
  (require 'evil-magit)
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-mode 1)
  (core-evil/setup-leader)
  (core-evil/setup-macros))

(defun core-evil/setup-leader ()
  "Setup evil mode leader key."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f"   'find-file
    "t"   'dired
    "w"   'save-buffer
    "ga"  'stage-current-buffer
    "gs"  'magit-status
    "gc"  'magit-commit-create
    "gl"  'magit-log-all
    "gps" 'magit-push-current-to-upstream))

(defun core-evil/setup-macros ()
  "Setup evil macros mapping to use it more quickly."
  (evil-define-key 'normal 'global
    "Q" "@q")
  (evil-define-key 'visual 'global
    "Q" (kbd ":norm @q RET")
    "." (kbd ":norm . RET")))

(provide 'core-evil)
