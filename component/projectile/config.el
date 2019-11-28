;;; config.el - Projectile Component File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: projectile

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

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice projectile-grep (after delete-grep-header activate) (delete-grep-header))

(defun projectile/init-projectile ()
  (require 'projectile)
  (setq projectile-globally-ignored-directories
    '(".idea"
      ".ensime_cache"
      ".eunit"
      ".git"
      ".hg"
      ".fslckout"
      "_FOSSIL_"
      ".bzr"
      "_darcs"
      ".tox"
      ".svn"
      ".stack-work"
      "node_modules"
      "composer")))

(defun projectile/init-helm-projectile ()
  (require 'helm-projectile)
  (helm-projectile-on)
  (global-set-key (kbd "C-x g") 'projectile-grep))
