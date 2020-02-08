;;; config.el --- Ivy Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: ivy counsel

;; This file is not part of GNU Emacs.

;; This Emacs config is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This Emacs config is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this Emacs config. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defun ivy/init-ivy ()
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap nil
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer nil
        ivy-extra-directories ())

  ;; Change the maximum width of the Ivy window to 1/3
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))

  (ivy//setup-keybindings)
  (add-hook 'minibuffer-setup-hook 'ivy//resize-minibuffer-setup-hook))

(defun ivy/init-ivy-prescient ()
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-find-file
               ivy-switch-buffer))
  (ivy-prescient-mode 1))

(defun ivy/init-prescient ()
  (require 'prescient)
  (setq prescient-history-length 200)
  (setq prescient-save-file (concat embla-temporary-directory "prescient-items"))
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode))

(defun ivy//setup-keybindings ()
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x d") 'counsel-dired)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c C-j") 'counsel-imenu)
  (global-set-key (kbd "C-x r l") 'counsel-bookmark)

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done))

(defun ivy//resize-minibuffer-setup-hook ()
  "Minibuffer setup hook."
  (add-hook 'post-command-hook #'ivy//resize-post-commad-hook nil t))

(defun ivy//resize-post-commad-hook ()
  "Hook run every command in minibuffer."
  (when ivy-mode
    (shrink-window (1+ ivy-height))))
