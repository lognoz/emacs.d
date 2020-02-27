;;; config.el --- Embla Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: embla

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

(with-eval-after-load "evil"
  (mode-line-initialize))

(defun embla/init-editorconfig ()
  (editorconfig-mode 1))

(defun embla/init-which-key ()
  (which-key-mode)
  (which-key-setup-minibuffer))

(defun embla/init-multiple-cursors ()
  (require 'multiple-cursors)

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(defun embla/init-orglink ()
  (global-orglink-mode))

(defun embla/init-pdf-tools ()
  (pdf-tools-install))

(defun embla/init-origami ()
  (global-origami-mode)
  (define-key evil-normal-state-map (kbd "TAB") 'origami-toggle-node)
  (global-set-key (kbd "<s-tab>") 'origami-toggle-all-nodes))
