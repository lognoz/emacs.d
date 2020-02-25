;;; config.el - Isearch Component File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: isearch

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

(defun isearch/init-isearch ()
  (setq search-highlight t
        search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil
        isearch-lazy-highlight t
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil
        isearch-yank-on-move 'shif
        isearch-allow-scroll 'unlimited)

  (isearch//setup-keybindings))

(defun isearch//setup-keybindings ()
  (define-key isearch-mode-map (kbd "DEL") 'isearch//isearch-abort))

(defun isearch//isearch-abort ()
  "Remove non-matching `isearch' input, reverting to previous
successful search and continuing with the search."
  (interactive)
  (discard-input)
  (while (or (not isearch-success) isearch-error)
    (isearch-pop-state))
  (isearch-update))
