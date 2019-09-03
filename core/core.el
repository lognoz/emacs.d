(defconst user-core-directory (concat user-emacs-directory "core/")
  "The root directory of core files.")

(defconst user-module-directory (concat user-emacs-directory "module/")
  "The root directory for module files.")

(defun core/init ()
  "Perform startup initialization."
  (core/disable-gui)
  (core/setup-encoding)
  (core/setup-custom-file))

(defun core/setup-custom-file ()
  "Place the variables created by Emacs in custom.el."
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file))

(defun core/setup-encoding ()
  "Define charset and UTF-8 encoding."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode)))

(defun core/disable-gui ()
  "Disable GUI components."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(provide 'core)
