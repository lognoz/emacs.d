;;; lisp/macro.el --- define macros -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: macro helper

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

;;; --- Keybinding helpers

(defmacro set-keybindings (&rest bindings)
  "Fetch BINDINGS and attach keybindings to its keymap.

If you want to attach keybindings to a mode, you need to specify
the keymap you want to use. See example below:

(set-keybindings :map embla-mode-map \"M-x\" counsel-M-x)"
  (let ((keymap))
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (if (equal key :map)
            (setq keymap (eval def))
          (setq key (kbd key))
          (define-key keymap key def))))))

(defun unset-keybindings (keymap &rest bindings)
  "Unset BINDINGS defined in its KEYMAP.

(unset-keybindings 'latex-mode-hook \"C-c C-k\")"
  (while bindings
    (let ((key (kbd (pop bindings))))
      (define-key keymap key nil))))


;;; --- Hook helpers

(defmacro set-hook (hook &rest body)
  "Add to the value of HOOK the lambda function constructed with the BODY.

(set-hook 'latex-mode-hook (auto-fill-mode -1))"
  `(add-hook ,hook
    (lambda () ,@body)))


(provide 'macro)

;;; macro.el ends here
