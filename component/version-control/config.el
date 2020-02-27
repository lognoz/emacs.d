;;; config.el --- Version Control Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: version-control git

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

(defun version-control/init-magit ()
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit))

(defun version-control/init-git-gutter ()
  (global-git-gutter-mode t)

  (setq git-gutter:update-interval 2
        git-gutter:modified-sign "~"
        git-gutter:added-sign "+"
        git-gutter:deleted-sign "-"
        git-gutter:hide-gutter t
        git-gutter:ask-p nil
        git-gutter:hide-gutter t)

  (global-set-key (kbd "<s-up>") 'git-gutter:previous-hunk)
  (global-set-key (kbd "<s-down>") 'git-gutter:next-hunk))
