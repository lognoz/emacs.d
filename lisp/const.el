;;; lisp/const.el --- core constants -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: constant

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

;;; --- Environmental constants

(defconst operating-system
  (cond ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'darwin) "mac")
        ((eq system-type 'windows-nt) "windows")
        ((eq system-type 'berkeley-unix) "bsd"))
  "The operating system running.")

(defconst current-user
  (getenv (if (eq operating-system "windows") "USERNAME" "USER"))
  "The current logged user.")

(defconst bsd-p
  (string-equal "bsd" operating-system)
  "Non-nil if it's BSD system.")

(defconst xorg-p
  (eq window-system 'x)
  "Non-nil if xorg is running as window system.")

(defconst root-p
  (string-equal "root" current-user)
  "Non-nil if Emacs is running by root user")

(defconst archlinux-p
  (string-match-p "ARCH" operating-system-release)
  "Non-nil if it's Arch Linux system.")

(defconst emacs-version-above-27-p
  (>= emacs-major-version 27)
  "Non-nil if Emacs is 27 or above.")


;;; --- Contextual Embla constants

(defconst embla-init-file (expand-file-name "init.el" user-emacs-directory)
  "The int file reference.")

(defconst embla-core-directory (expand-file-name "core/" user-emacs-directory)
  "The directory of core files.")

(defconst embla-private-directory (expand-file-name "private/" user-emacs-directory)
  "The directory of private files.")

(defconst embla-temporary-directory (expand-file-name "temporary/" embla-private-directory)
  "The directory of temporary files.")

(defconst embla-private-init-file (expand-file-name "init.el" embla-private-directory)
  "The private initialization file.")

(defconst embla-autoload-file (expand-file-name "embla-autoload.el" embla-temporary-directory)
  "The main autoload file.")


(provide 'const)

;;; const.el ends here
