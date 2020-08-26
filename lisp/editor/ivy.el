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

(require-package 'counsel
                 'ivy)


;;;###autoload
(defun counsel-initialize ()
  "Sets counsel configurations."
  (setq counsel-find-file-ignore-regexp
        (concat
          ;; File names beginning with # or .
          "\\(?:\\`[#.]\\)"
          ;; File names ending with # or ~
          "\\|\\(?:\\`.+?[#~]\\'\\)")))

;;;###autoload
(defun ivy-initialize ()
  "Sets ivy configurations."
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap nil)
  (setq ivy-on-del-error-function nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-fixed-height-minibuffer nil)

  ;; Sets ivy with `projectile'.
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Sets ivy with `magit'.
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (ivy-mode t))

;;;###autoload
(defun ivy-format-initialize ()
  "Changes the way to output ivy candidates."
  (with-eval-after-load 'ivy
    (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
    (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))
    (add-hook 'minibuffer-setup-hook 'ivy-resize-minibuffer-setup-hook)))

(defun ivy-format-function-arrow (cands)
  "Transforms CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(defun ivy-resize-minibuffer-setup-hook ()
  "Sets local hook to when a minibuffer is open."
  (add-hook 'post-command-hook #'ivy-resize-post-commad-hook nil t))

(defun ivy-resize-post-commad-hook ()
  "Resizes the minibuffer on `post-command-hook'."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))


;;;###autoload
(let ((map embla-mode-map))
  (define-key map (kbd "M-x") 'counsel-M-x)
  (define-key map (kbd "C-x C-f") 'counsel-find-file)
  (define-key map (kbd "C-c g") 'counsel-git)
  (define-key map (kbd "C-c j") 'counsel-git-grep)
  (define-key map (kbd "C-c a") 'counsel-ag)
  (define-key map (kbd "C-x l") 'counsel-locate)
  (define-key map (kbd "M-y") 'counsel-yank-pop)
  (define-key map (kbd "C-x d") 'counsel-dired)
  (define-key map (kbd "C-x f") 'counsel-recentf)
  (define-key map (kbd "C-x C-f") 'counsel-find-file)
  (define-key map (kbd "C-c C-j") 'counsel-imenu)
  (define-key map (kbd "C-x r l") 'counsel-bookmark))

;;;###autoload
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done))


;;;###autoload
(defer-loading
  :event
  pre-command-hook
  :function
  ivy-initialize
  ivy-format-initialize
  counsel-initialize)


;;; ivy.el ends here
