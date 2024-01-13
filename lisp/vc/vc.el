;;; list/editor/vc.el --- Extensions to version-control -*- lexical-binding: t -*-

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

;; This is an extension for better version-control configuration.

;;; Code:

(embla-elpa-package 'magit)

;; (embla-elpa-package 'git-gutter+)

;;;###autoload
(let ((map embla-mode-map))
  (define-key map (kbd "<s-up>") #'git-gutter:previous-hunk)
  (define-key map (kbd "<s-down>") #'git-gutter:next-hunk)
  (define-key map (kbd "C-x g") #'magit))

;; ;;;###autoload
;; (embla-elpa-package 'git-gutter
;;   (global-git-gutter-mode t)
;;   (setq git-gutter:update-interval 2)
;;   (setq git-gutter:modified-sign "~")
;;   (setq git-gutter:added-sign "+")
;;   (setq git-gutter:deleted-sign "-")
;;   (setq git-gutter:hide-gutter t)
;;   (setq git-gutter:ask-p nil)
;;   (setq git-gutter:hide-gutter t))

;;; vc.el ends here
