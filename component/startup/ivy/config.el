;;; config.el --- Ivy Component Config File

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

(defun ivy-init-ivy ()
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "(%d/%d) "
        ivy-wrap nil
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer nil)

  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  ;; Change the maximum width of the Ivy window to 1/3
  (setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 3))))
  (add-hook 'minibuffer-setup-hook 'ivy-resize-minibuffer-setup-hook))

(defun ivy-init-counsel ()
  (setq counsel-yank-pop-preselect-last t))

(defun ivy-init-ivy-prescient ()
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (setq ivy-prescient-sort-commands
    '(:not counsel-dired
           counsel-grep
           counsel-rg
           counsel-find-file
           counsel-yank-pop
           ivy-switch-buffer)))

(defun ivy-init-prescient ()
  (require 'prescient)
  (setq prescient-history-length 200)
  (setq prescient-save-file (concat embla-temporary-directory "prescient"))
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode))
