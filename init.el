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

;; Change the frequency of garbage collection for better launch time.
(setq gc-cons-threshold 100000000)

;; Show warning when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

(if (version< emacs-version "25.1")
  (error "Embla requires GNU Emacs 25.1 or newer, but you're running %s"
         emacs-version)
  (setq user-emacs-directory (file-name-directory load-file-name))
  (load (concat user-emacs-directory "core/core-embla")
        nil 'nomessage)
  (embla/initialize))
