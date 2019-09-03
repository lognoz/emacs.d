;;; core-version-control.el - Core Version Control File
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

(defun core-version-control/init ()
  "Perform core version control initialization."
  (add-package (git-gutter git-gutter+))
  (core-version-control/setup-git-gutter))

(defun core-version-control/setup-git-gutter ()
  "Setup git gutter mode."
  (global-git-gutter-mode t)
  (setq git-gutter:update-interval 2
        git-gutter:modified-sign "~"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:hide-gutter t
        git-gutter:ask-p nil
        git-gutter:hide-gutter t))

(provide 'core-version-control)
