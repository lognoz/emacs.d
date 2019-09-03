;;; init.el - Initialization File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(defun load-dir (dir) 
  "This function help to load directories."
  (add-to-list 'load-path dir))

(defconst user-core-directory (concat user-emacs-directory "core/")
  "The root directory of core files.")

(defconst user-module-directory (concat user-emacs-directory "module/")
  "The root directory for module files.")

;; Load user directories.
(load-dir user-core-directory)
(load-dir user-module-directory)

;; Perform initialization.
(require 'core)
(core/init)
