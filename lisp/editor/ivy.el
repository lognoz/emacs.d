;;; list/editor/ivy.el --- Extensions to ivy -*- lexical-binding: t -*-

;; Copyright (c) Marc-Antoine Loignon <developer@lognoz.org>

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/embla
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an extension for better ivy configuration.

;;; Code:

(require 'project)

;;;###autoload
(embla-autoload "editor/ivy" pre-command-hook)

;;;; Ivy configurations

(embla-elpa-package 'counsel
  (setq counsel-find-file-ignore-regexp
        (concat
          ;; File names beginning with # or .
          "\\(?:\\`[#.]\\)"
          ;; File names ending with # or ~
          "\\|\\(?:\\`.+?[#~]\\'\\)")))

(embla-elpa-package 'ivy
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "")
  (setq ivy-wrap nil)
  (setq ivy-on-del-error-function nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-fixed-height-minibuffer nil)
  (setq ivy-display-style nil)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-extra-directories '("./"))

  ;; Enable ivy mode.
  (ivy-mode t)

  ;; Setup ivy with `magit'.
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

;;;###autoload
(let ((map embla-mode-map))
  (define-key map (kbd "M-x") #'counsel-M-x)
  (define-key map (kbd "C-x b") #'counsel-switch-buffer)
  (define-key map (kbd "C-c g") #'counsel-git)
  (define-key map (kbd "C-c j") #'counsel-git-grep)
  (define-key map (kbd "C-c a") #'counsel-ag)
  (define-key map (kbd "C-x l") #'counsel-locate)
  (define-key map (kbd "M-y") #'counsel-yank-pop)
  (define-key map (kbd "C-x d") #'counsel-dired)
  (define-key map (kbd "C-x C-f") #'counsel-find-file)
  (define-key map (kbd "C-c C-j") #'counsel-imenu)
  (define-key map (kbd "C-x r l") #'counsel-bookmark))

(let ((map ivy-minibuffer-map))
  (define-key map (kbd "<tab>") #'ivy-alt-done)
  (define-key map (kbd "M-j") #'embla-counsel-switch-to-jump)
  (define-key map (kbd "C-j") #'embla-counsel-switch-to-jump)
  (define-key map (kbd "M-.") #'embla-counsel-goto-project-root)
  (define-key map (kbd "C-.") #'embla-counsel-goto-project-root))

;;;; Minibuffer

(with-eval-after-load 'ivy
  (setf (alist-get 't ivy-format-functions-alist) #'embla-ivy-format-function)
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))
  (add-hook 'minibuffer-setup-hook #'embla-ivy-resize-minibuffer-setup-hook))

(defun embla-ivy-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "-> " (ivy--add-face str (face-attribute 'bold :weight))))
   (lambda (str)
     (concat "   " str))
   cands
   "\n"))

(defun embla-ivy-resize-minibuffer-setup-hook ()
  "Attach local hook to when a minibuffer is open."
  (add-hook 'post-command-hook #'embla-ivy-resize-post-commad-hook nil t))

(defun embla-ivy-resize-post-commad-hook ()
  "Resize the minibuffer on `post-command-hook'."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))

;;;; Jump function support in `find-file'

(defconst embla-counsel-files-symbols-alist
  '(counsel-find-file
    counsel-dired
    counsel-dired-jump
    counsel-file-jump)
  "List of `ivy-state-caller' used for files management.")

(defun embla--counsel-directory-in-minibuffer ()
  "Return the current directory in minibuffer."
  (if ivy--directory
      (directory-file-name (expand-file-name ivy--directory))
    default-directory))

(defun embla--counsel-run-file-prompt (func dir)
  "Quit and run file symbol FUNC dynamically.
If there is a DIR, the function use it as default prompt value."
  (ivy-quit-and-run
    (if (string-match "-jump\\'" (symbol-name func))
        (funcall func "" dir)
      (funcall func dir))))

(defun embla-counsel-project-find-file ()
  "Open `find-file' on project root."
  (interactive)
  (let ((root (project-root (project-current t))))
    (counsel-find-file
     (if root root default-directory))))

(defun embla-counsel-switch-to-jump ()
  "Switch prompt to its jump function."
  (interactive)
  (let ((func (ivy-state-caller ivy-last))
        (dir (embla--counsel-directory-in-minibuffer)))
    (when (member func embla-counsel-files-symbols-alist)
      (setq func (symbol-name func))
      (embla--counsel-run-file-prompt
        (intern
          (if (string-match "-jump\\'" func)
              (if (string-equal func "counsel-file-jump")
                  "counsel-find-file"
                (string-trim-right func "-jump\\'"))
            (if (string-equal func "counsel-find-file")
                "counsel-file-jump"
              (concat func "-jump"))))
        dir))))

(defun embla-counsel-goto-project-root ()
  "Change the current location to project root on ivy minibuffer."
  (interactive)
  (let* ((func (ivy-state-caller ivy-last))
         (dir (counsel-directory-in-minibuffer))
         (root (project-root (project-current t))))
    (when (and root (member func embla-counsel-files-symbols-alist))
      (embla--counsel-run-file-prompt func root))))

;;; ivy.el ends here
