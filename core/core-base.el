;;; core-base.el - Core Base Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: base core

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

;; Change startup pages
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Highlight the current line
(global-hl-line-mode)

;; Use pair mode
(electric-pair-mode 1)

;; Change yes or no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Define better feedbacks configurations.
(setq vc-follow-symlinks t
      x-stretch-cursor nil
      echo-keystrokes 0.25
      auto-revert-verbose nil)

;; Define ido mode
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 8)

(ido-mode 1)
(ido-everywhere 1)

(provide 'core-base)
