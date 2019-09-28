;;; config.el - Helm Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: helm

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

(defun helm/init-helm ()
  (require 'helm)
  (require 'helm-config)
  (require 'helm-projectile)

  (helm-autoresize-mode 1)
  (helm-projectile-on)
  (helm-mode 1)

  (setq helm-candidate-number-limit 50
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer
        helm-semantic-fuzzy-match t
	      helm-imenu-fuzzy-match t

        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil

        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil

        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25

        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil

        ;; Disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)

  (advice-add 'helm-ff-filter-candidate-one-by-one
    :around (lambda (fcn file)
              (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
                (funcall fcn file))))

  (helm//setup-helm-files)
  (helm//setup-keybindings))

(defun helm//setup-helm-files ()
  (with-eval-after-load 'helm-files
    (define-key helm-read-file-map (kbd "<backspace>") 'find-files-up-one-level)
    (define-key helm-read-file-map (kbd "DEL") 'find-files-up-one-level)
    (define-key helm-find-files-map (kbd "<backspace>") 'find-files-up-one-level)
    (define-key helm-find-files-map (kbd "DEL") 'find-files-up-one-level))

  (defun find-files-up-one-level ()
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-char -1))))

(defun helm//setup-keybindings ()
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-find-files-map (kbd "<C-backspace>") 'helm-find-files-up-one-level))
