;;; lisp/editor/vc.el --- version control configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: version control

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require-package 'magit
                 'evil-magit
                 'git-gutter
                 'git-gutter+
                 'git-timemachine
                 'gitattributes-mode
                 'gitconfig-mode
                 'gitignore-mode)

;;; --- Magit configuration

(with-eval-after-load 'magit
    (require 'evil-magit))


;;; --- Git gutter configuration

(setq git-gutter:update-interval 2
      git-gutter:modified-sign "~"
      git-gutter:added-sign "+"
      git-gutter:deleted-sign "-"
      git-gutter:hide-gutter t
      git-gutter:ask-p nil
      git-gutter:hide-gutter t)

(global-git-gutter-mode t)


;;; --- Version control keybindings

(set-keybindings
  :map        embla-mode-map
  "<s-up>"    git-gutter:previous-hunk
  "<s-down>"  git-gutter:next-hunk
  "C-x g"     magit-status
  "C-x M-g"   magit-dispatch
  "C-c M-g"   magit-file-popup)
