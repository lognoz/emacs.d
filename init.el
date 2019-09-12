;;; init.el --- Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: init

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

(defun load-dir (dir)
  "This function help to load directories."
  (add-to-list 'load-path dir))

(defconst user-core-directory (concat user-emacs-directory "core/")
  "The root directory of core files.")

(defconst user-component-directory (concat user-emacs-directory "component/")
  "The root directory for component files.")

(defconst user-language-directory (concat user-emacs-directory "language/")
  "The root directory for language files.")

(defconst user-temporary-directory (concat user-emacs-directory "tmp/")
  "The root directory for temporary files.")

(defconst user-environment-directory (concat user-emacs-directory "environment/" user-login-name "/")
  "The root directory for environment files. It use user login name to define it")

(defconst user-init-file (concat user-environment-directory "init.el")
  "The root file for custom init.el by user login name.")

; Load user directories.
(load-dir user-core-directory)
(load-dir user-component-directory)
(load-dir user-language-directory)
(load-dir user-environment-directory)

;; Perform initialization.
(require 'core)
(core/init)

;; Load custom initialization by user.
(if (file-exists-p user-init-file)
    (load user-init-file))
