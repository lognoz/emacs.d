;;; .dir-locals.el --- Directory local File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: local variable

;; This file specify local variable values common to all files
;; in that directory; Emacs uses these to create buffer-local bindings
;; for those variables in buffers visiting any file in that
;; directory.

;;; Code:

((nil
  (mode . embla)
  (project-sources .
    ("https://github.com/lognoz/embla"
     "https://github.com/lognoz/embla/issues")))
 (emacs-lisp-mode
  (indent-tabs-mode . nil)))
