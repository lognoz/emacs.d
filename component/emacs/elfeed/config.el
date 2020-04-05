;;; config.el --- Elfeed Component Config File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: elfreed

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

;;; Contextual core variables.

(defvar elfeed-web-loader-hooks '(elfeed-search-mode-hook)
  "The hook that load elfeed web module.")

;;; Internal core functions.

(defun elfeed-init-elfeed ()
  (setq elfeed-use-curl t
        elfeed-curl-max-connections 10
        elfeed-search-clipboard-type 'CLIPBOARD
        elfeed-search-title-max-width (current-fill-column)
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30
        elfeed-search-trailing-width 16
        elfeed-show-truncate-long-urls t
        elfeed-show-unique-buffers t)

  (evil-collection-elfeed-setup)
  (elfeed-update))
