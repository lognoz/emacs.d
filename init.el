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
