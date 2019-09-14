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
  "This function load files in directories."
  (add-to-list 'load-path dir))

(defconst user-core-directory (concat user-emacs-directory "core/")
  "The root directory of core files.")

(defconst user-component-directory (concat user-emacs-directory "component/")
  "The root directory for component files.")

(defconst user-temporary-directory (concat user-emacs-directory "temporary/")
  "The root directory for temporary files.")

(defconst user-environment-directory (concat user-emacs-directory "environment/" user-login-name "/")
  "The root directory for environment files. It use user login name to define it")

;; Make sure Embla can be run in Emacs
(defconst emacs-min-version "25")
(if (not (version<= emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old for Embla."
                   "Make sure to have version %s or above of Emacs.")
           emacs-version emacs-min-version))

;; Load user directories.
(load-dir user-core-directory)
(load-dir user-component-directory)
(load-dir user-environment-directory)

;; Perform initialization.
(require 'core-embla)
(core-init)

;; Load custom initialization by user connected.
(setq path (concat user-environment-directory "init.el"))
(when (file-exists-p path)
      (load path))

;; Start Emacs server
(require 'server)
(unless (server-running-p) (server-start))
