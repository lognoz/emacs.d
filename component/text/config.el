;;; packages.el - Text Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: text

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

;; System requirements:
;;
;; If you are on Archlinux, you will need to execute this command:
;; sudo pacman -S ispell aspell-en

(defun text/init-text-mode ()
  (add-hook 'text-mode-hook (lambda ()
    (flyspell-mode 1))))

(defun text/init-evil ()
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map "z=" 'helm-flyspell-correct)))
