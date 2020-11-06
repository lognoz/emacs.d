;;; lisp/tools/grammar.el --- grammar configurations -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: grammar

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

;;;###autoload
(eval-before-init
  ;; Use this emacs langtool version instead of the one in melpa
  ;; because we can use the program in `prog-mode' and `org-mode'.
  (unless (directory-in-site-p "Emacs-langtool")
    (clone-repository "https://github.com/redguardtoo/Emacs-langtool")))

;;;###autoload
(defcustom embla-enable-flyspell t
  "Non-nil if Embla need to enable flyspell."
  :group 'embla
  :type 'boolean)

(defvar embla-correcting-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x c d") 'langtool-check-done)
    (define-key map (kbd "C-x c c") 'langtool-check)
    (define-key map (kbd "C-x c a") 'langtool-correct-buffer)
    map))

(define-minor-mode embla-correcting-mode
  "Minor mode that make documentation correction."
  :global nil
  :keymap 'embla-correcting-mode-map
  (if embla-correcting-mode
      (progn
        (langtool-check)
        (add-hook 'after-save-hook 'langtool-check nil 'make-it-local))
    (langtool-check-done)
    (remove-hook 'after-save-hook 'langtool-check)))

(defvar ctl-x-c-map (make-sparse-keymap)
  "Keymap for subcommands of C-x c.")

(defalias 'ctl-x-c-prefix ctl-x-c-map)
(define-key ctl-x-map "c" 'ctl-x-c-prefix)
(define-key ctl-x-c-map "m" 'embla-correcting-mode)

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
  'face-defface-spec)

;;;###autoload
(define-component embla-langtool ()
  "Setup langtool configurations."
  (setup-language-tool)
  (eval-after-load 'prog-mode
    (setup-prog-language-tool)))

;;;###autoload
(define-component embla-flyspell-text (text-mode-hook)
  "Setup flyspell for `text-mode'."
  (when embla-enable-flyspell
    (flyspell-mode 1)
    (message "")))

;;;###autoload
(define-component embla-flyspell-prog (prog-mode-hook)
  "Setup flyspell for `prog-mode'."
  (when embla-enable-flyspell
    (flyspell-prog-mode)
    (message "")))

;;;###autoload
(defun setup-language-tool ()
  "Setup language tool configurations."
  (setq langtool-default-language "en-US")
  (setq langtool-http-server-stream-type 'tls)
  (when linux-p
    (setq langtool-java-classpath
          "/usr/share/languagetool:/usr/share/java/languagetool/*")))

;;;###autoload
(defun setup-prog-language-tool ()
  "Setup language tool predicate for `prog-mode'."
  '(progn
     (unless (featurep 'flyspell)
       (require 'flyspell))
     (setq langtool-generic-check-predicate
           '(lambda (start end)
              (let ((f (get-text-property start 'face)))
                (memq f flyspell-prog-text-faces))))))

;;; grammar.el ends here
