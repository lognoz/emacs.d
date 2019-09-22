;;; component-autocomplete.el - Autocomplete Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: autocomplete company snippet

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

(defun component-autocomplete/install ()
  "Install compagny requirements."
  (add-package (company yasnippet helm))
  (add-hook 'after-init-hook 'global-company-mode))

(defun component-autocomplete/setup-configuration ()
  "Set company mode configurations."
  (setq company-minimum-prefix-length 1
        company-idle-delay 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)))

(component-autocomplete/install)
(component-autocomplete/setup-configuration)

(require 'yasnippet)
(require 'company)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(with-eval-after-load 'company
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))


(require 'helm)
(require 'helm-config)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)

(helm-mode 1)
(helm-autoresize-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(define-key helm-find-files-map (kbd "<C-backspace>") 'helm-find-files-up-one-level)

(require 'cl-lib)

(defvar no-dots-whitelist
  '()
  "List of helm buffers in which to show dots.")

(defun no-dots/whitelistedp ()
  (member (with-helm-buffer (buffer-name)) no-dots-whitelist))

(defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
  (when (or (no-dots/whitelistedp)
            (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
    (funcall fcn file)))

(defun no-dots/helm-file-completion-source-p (&rest args) t)

(defun no-dots/helm-attrset (fcn attribute-name value &optional src)
  (let ((src (or src (helm-get-current-source))))
    (when src
      (funcall fcn attribute-name value src))))

(defun no-dots/helm-find-files-up-one-level (fcn &rest args)
  (advice-add 'helm-file-completion-source-p
              :around 'no-dots/helm-file-completion-source-p)
  (advice-add 'helm-attrset
              :around 'no-dots/helm-attrset)
  (let ((res (apply fcn args)))
    (advice-remove 'helm-file-completion-source-p
                   'no-dots/helm-file-completion-source-p)
    (advice-remove 'helm-attrset
                   'no-dots/helm-attrset)
    res))

(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :around 'no-dots/helm-ff-filter-candidate-one-by-one)
  (advice-add 'helm-find-files-up-one-level
              :around 'no-dots/helm-find-files-up-one-level))

(defadvice helm-display-mode-line (after undisplay-header activate)
  (setq header-line-format nil))

(setq helm-find-files-doc-header "")

(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

(provide 'component-autocomplete)
