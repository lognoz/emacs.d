;;; lisp/core-vars.el --- core variables -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: util variable

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


(provide 'core-vars)

;; Local Variables:
;; no-update-autoloads: t
;; End:

;;; core-vars.el ends here
