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

;;(defun delete-grep-header ()
;;  (save-excursion
;;    (with-current-buffer grep-last-buffer
;;      (goto-line 5)
;;      (narrow-to-region (point) (point-max)))))
;;
;;(defadvice projectile-grep (after delete-grep-header activate) (delete-grep-header))
;; https://www.reddit.com/r/emacs/comments/29zm3q/how_to_get_rid_of_filename_files_that_emacs_is/

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
       "composer"))

  (add-hook 'projectile-mode-hook 'projectile//init-environment))

(defun projectile/init-helm-projectile ()
  (require 'helm-projectile)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(defun projectile//init-environment ()
  (define-key projectile-mode-map (kbd "C-x C-f") 'helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-x d") 'helm-projectile-find-dir)
  (define-key projectile-mode-map (kbd "C-x b") 'helm-projectile-switch-to-buffer))
