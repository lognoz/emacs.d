;;; lisp/editor/ivy.el --- ivy configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: ivy

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;###autoload
(boot-packages 'projectile
               'counsel
               'ivy)


;;;###autoload
(advice pre-command-hook
        setup-ivy
        setup-counsel
        ivy-mode
        after-boot-ivy)

;;;###autoload
(bind-keys embla-mode-map
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g"   . counsel-git)
  ("C-c j"   . counsel-git-grep)
  ("C-c a"   . counsel-ag)
  ("C-x l"   . counsel-locate)
  ("M-y"     . counsel-yank-pop)
  ("C-x d"   . counsel-dired)
  ("C-x f"   . counsel-recentf)
  ("C-x C-f" . counsel-find-file)
  ("C-c C-j" . counsel-imenu)
  ("C-x r l" . counsel-bookmark))

(defconst ivy-files-prompt
  '(counsel-find-file
    counsel-dired
    counsel-dired-jump
    counsel-file-jump)
  "List of `ivy-state-caller' used for files management.")


;;;###autoload
(defun setup-counsel ()
  "Setup counsel configurations."
  (setq counsel-find-file-ignore-regexp
        (concat
          ;; File names beginning with # or .
          "\\(?:\\`[#.]\\)"
          ;; File names ending with # or ~
          "\\|\\(?:\\`.+?[#~]\\'\\)")))

;;;###autoload
(defun setup-ivy ()
  "Setup ivy configurations."
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "")
  (setq ivy-wrap nil)
  (setq ivy-on-del-error-function nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-fixed-height-minibuffer nil)
  (setq ivy-display-style nil)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-extra-directories nil)

  ;; Setup ivy with `projectile'.
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Setup ivy with `magit'.
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Setup the way to output ivy candidates.
  (with-eval-after-load 'ivy
    (setf (alist-get 't ivy-format-functions-alist) #'ivy-embla-format-function)
    (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))
    (add-hook 'minibuffer-setup-hook 'ivy-resize-minibuffer-setup-hook)))

;;;###autoload
(defun after-boot-ivy ()
  "Setup ivy keybindings after it's started."
  (bind-keys ivy-minibuffer-map
    ("<tab>" . ivy-alt-done)
    ("M-f"   . counsel-switch-to-jump)
    ("M-."   . counsel-project-root)
    ("C-."   . counsel-project-root)))

(defun ivy-embla-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "-> " (ivy--add-face str (face-attribute 'bold :weight))))
   (lambda (str)
     (concat "   " str))
   cands
   "\n"))

(defun ivy-resize-minibuffer-setup-hook ()
  "Attach local hook to when a minibuffer is open."
  (add-hook 'post-command-hook #'ivy-resize-post-commad-hook nil t))

(defun ivy-resize-post-commad-hook ()
  "Resize the minibuffer on `post-command-hook'."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))

(defun counsel-project-root ()
  "Navigate to project root on ivy minibuffer."
  (interactive)
  (unless (bound-and-true-p projectile-mode)
    (projectile-mode t))
  (let* ((ivy-caller (ivy-state-caller ivy-last))
         (directory (if ivy--directory
                        (directory-file-name (expand-file-name ivy--directory))
                      default-directory))
         (root (projectile-project-root directory)))
    (when (and root (member ivy-caller ivy-files-prompt))
      (ivy-quit-and-run
        (cond ((equal ivy-caller 'counsel-find-file)
               (counsel-find-file root))
              ((equal ivy-caller 'counsel-dired)
               (counsel-dired root))
              ((equal ivy-caller 'counsel-file-jump)
               (counsel-file-jump "" root))
              ((equal ivy-caller 'counsel-dired-jump)
               (counsel-dired-jump "" root)))))))

(defun counsel-switch-to-jump ()
  "Switch prompt to its jump function."
  (interactive)
  (let ((ivy-caller (ivy-state-caller ivy-last))
        (directory (if ivy--directory
                       (directory-file-name (expand-file-name ivy--directory))
                     default-directory)))
    (when (member ivy-caller ivy-files-prompt)
      (ivy-quit-and-run
        (cond ((equal ivy-caller 'counsel-find-file)
               (counsel-file-jump "" directory))
              ((equal ivy-caller 'counsel-dired)
               (counsel-dired-jump "" directory))
              ((equal ivy-caller 'counsel-file-jump)
               (counsel-find-file directory))
              ((equal ivy-caller 'counsel-dired-jump)
               (counsel-dired directory)))))))

;;; ivy.el ends here