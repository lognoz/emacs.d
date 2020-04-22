;;; config.el --- Evil Component Config File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: evil

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

(defun evil-init-evil ()
  (setq evil-toggle-key "C-`"
        evil-want-keybinding nil)

  (global-evil-surround-mode 1)
  (evil-mode 1))

(defun evil-init-evil-smartparens ()
  (require 'smartparens-config)
  (smartparens-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(defun evil-init-evil-magit ()
  (with-eval-after-load 'magit
    (require 'evil-magit)))
