;;; lisp/textmodes/spell-checker.el --- Spell checker configurations -*- lexical-binding: t -*-

;; Copyright (C)  Marc-Antoine Loignon <developer@lognoz.org>

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; URL: https://github.com/lognoz/embla
;; Keywords: grammar style spell checker

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an extension for grammar, style, and spell checker.

;;; Code:

;;;###autoload
(embla-autoload "textmodes/spell-checker" after-find-file dired-initial-position-hook)

;;;; Correcting mode

(defvar embla-correcting-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x c d") #'langtool-check-done)
    (define-key map (kbd "C-x c c") #'langtool-check)
    (define-key map (kbd "C-x c a") #'langtool-correct-buffer)
    map))

(defvar ctl-x-c-map (make-sparse-keymap)
  "Keymap for subcommands of C-x c.")

(define-minor-mode embla-correcting-mode
  "Minor mode that make documentation correction."
  :global nil
  :keymap embla-correcting-mode-map
  (if embla-correcting-mode
      (progn
        (langtool-check)
        (add-hook 'after-save-hook #'langtool-check nil 'make-it-local))
    (langtool-check-done)
    (remove-hook 'after-save-hook #'langtool-check)))

(defalias 'ctl-x-c-prefix ctl-x-c-map)
(define-key ctl-x-map "c" #'ctl-x-c-prefix)
(define-key ctl-x-c-map "m" #'embla-correcting-mode)

;;;; Flyspell support

(defcustom embla-enable-flyspell t
  "Non-nil if Embla need to enable flyspell."
  :group 'embla
  :type 'boolean)

;;;###autoload (add-hook 'text-mode-hook #'embla-enable-flyspell)
;;;###autoload (add-hook 'prog-mode-hook #'embla-enable-flyspell)
;;;###autoload
(defun embla-enable-flyspell ()
  "Enable flyspell based on derived mode."
  (when embla-enable-flyspell
    (if (derived-mode-p 'prog-mode)
        (flyspell-prog-mode)
      (flyspell-mode 1))
    (message "")))

;;;; Languagetool support

;; Use this emacs langtool version instead of the one in melpa
;; because we can use the program in `prog-mode' and `org-mode'.

(embla-site-lisp-package "https://github.com/redguardtoo/Emacs-langtool"
  (setq langtool-default-language "en-US")
  (setq langtool-http-server-stream-type 'tls)
  (setq langtool-java-classpath
        (cond ((eq system-type 'gnu/linux)
               "/usr/share/languagetool:/usr/share/java/languagetool/*")
              ((eq system-type 'berkeley-unix)
               "/usr/local/share/languagetool:/usr/local/share/java/classes/languagetool/*")))

  (face-spec-set 'langtool-correction-face
    '((((supports :underline (:style wave)))
      :underline (:style wave :color "DarkOrange"))
      (t
      :underline t :inherit error))
    'face-defface-spec)

  (face-spec-set 'langtool-errline
    '((((supports :underline (:style wave)))
      :underline (:style wave :color "DarkOrange"))
      (t :underline t :inherit error))
    'face-defface-spec))

(eval-after-load 'prog-mode
  '(progn
    (unless (featurep 'flyspell)
      (require 'flyspell))
    (setq langtool-generic-check-predicate
          '(lambda (start end)
            (let ((f (get-text-property start 'face)))
              (memq f flyspell-prog-text-faces))))))

;;; spell-checker.el ends here
