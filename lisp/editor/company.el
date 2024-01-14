G;;; lisp/editor/company.el --- Extensions to company -*- lexical-binding: t -*-

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

;; This is an extension for company.  This module enable Emacs
;; auto-completion.

;;; Code:

;;;###autoload
(embla-autoload "editor/company" pre-command-hook)

(embla-elpa-package 'company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.5)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend))

  ;; Enable company mode.
  (global-company-mode t)

  (let ((map company-active-map))
    (define-key map (kbd "<tab>") #'company-complete-selection)
    (define-key map (kbd "C-n") #'company-select-next)
    (define-key map (kbd "C-p") #'company-select-previous)))

;; (embla-site-lisp-package "https://github.com/Exafunction/codeium.el"
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(embla-elpa-package 'compat)

(embla-site-lisp-package "https://gitlab.com/daanturo/starhugger.el"
  (keymap-global-set "M-<tab>" #'starhugger-trigger-suggestion)

  (with-eval-after-load 'starhugger
    (starhugger-auto-mode t)

    (keymap-set starhugger-inlining-mode-map "TAB" (starhugger-inline-menu-item #'starhugger-accept-suggestion))
    (keymap-set starhugger-inlining-mode-map "M-[" (starhugger-inline-menu-item #'starhugger-show-prev-suggestion))
    (keymap-set starhugger-inlining-mode-map "M-]" (starhugger-inline-menu-item #'starhugger-show-next-suggestion))
    (keymap-set starhugger-inlining-mode-map "M-<tab>" (starhugger-inline-menu-item #'starhugger-show-next-suggestion))
    (keymap-set starhugger-inlining-mode-map "M-f" (starhugger-inline-menu-item #'starhugger-accept-suggestion-by-word))

    (defvar embla-evil-force-normal-state-hook '())
    (defun embla-evil-run-force-normal-state-hook-after-a (&rest _)
      (run-hooks 'embla-evil-force-normal-state-hook))

    (advice-add #'evil-force-normal-state
      :after #'embla-evil-run-force-normal-state-hook-after-a)))

(embla-elpa-package 'company-box
  (when (display-graphic-p)
    (company-box-mode t)
    (setq company-box-enable-icon nil)
    (setq company-box-backends-colors nil)
    (advice-add #'company-box--update-scrollbar :around #'embla-company-box-hide-scrollbar)))

(defun embla-company-box-hide-scrollbar (orig-fn &rest args)
  "Hide scrollbar when `company-box-mode' is actived."
  (cl-letf (((symbol-function #'display-buffer-in-side-window)
             (symbol-function #'ignore)))
    (apply orig-fn args)))

(defun embla-company-confirm-selection (&optional arg)
  "Confirm the selected candidate on company mode.
If there's no selection, this function will choose the first one."
  (interactive "p")
  (unless company-selection
    (company-select-next))
  (company-complete-selection))

;;; company.el ends here
