;;; core-package.el - Core Package File
;;
;; Copyright (c) 2019-2019 Marc-Antoine Loignon & Contributors
;;
;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/emacs.d
;;
;; This file is not part of GNU Emacs.

(defun stage-current-buffer ()
  "Stage changes of active buffer."
  (interactive)
  (let* ((buffile (buffer-file-name))
    (output (shell-command-to-string
            (concat "git add " (buffer-file-name)))))
    (message (if (not (string= output ""))
        output (concat "Added " buffile)))))

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

(defun core-package/setup-evil ()
  "Setup evil mode."
  (add-package (evil
                evil-collection
                evil-indent-plus
                evil-leader
                evil-magit
                evil-smartparens
                evil-surround))
  (global-evil-leader-mode 1)
  (global-evil-surround-mode 1)
  (evil-mode 1)
  (core-package/setup-evil-leader))

(defun core-package/setup-evil-leader ()
  "Setup evil mode leader key."
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f"  'find-file
    "t"  'dired
    "w"  'save-buffer
    "ga" 'stage-current-buffer
    "gs" 'magit-status
    "gc" 'magit-commit-create
    "gl" 'magit-log-all))

(provide 'core-package)
