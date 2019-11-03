;;; core-mode-line.el - Mode line Initialization File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: core mode-line

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

(require 'vc)

(defun mode-line--version-control ()
  (when (stringp vc-mode)
    (format "%s%s"
      (char-to-string #xe0a0)
      (replace-regexp-in-string
        (format "^ %s-" (vc-backend buffer-file-name)) " " vc-mode))))

(defun mode-line--buffer-name ()
  "Render a different buffer name if version control is active or not."
  (let ((text (buffer-name)) (face 'bold))
    (if buffer-file-name
      (when (and vc-mode buffer-file-name)
        (let* ((backend (vc-responsible-backend buffer-file-name))
               (vc-path (file-truename (vc-call-backend backend 'root buffer-file-name))))
          ;; Define text variable with buffer path with the version control location.
          (setq text (substring buffer-file-name (length vc-path)))))
      ;; If it's an Emacs system buffer change font style to italic.
      (setq face 'italic))
    (propertize (concat " " text) 'face face)))

(defun mode-line--evil-state ()
  (cond
    ((eq evil-state 'insert) "Insert")
    ((eq evil-state 'normal) "Normal")
    ((eq evil-state 'visual) "Visual")
    ((eq evil-state 'replace) "Replace")))

(defun face (text)
  (when (not (= (length text) 0))
    (concat " " text " ")))

(defun mode-line--fill (reserve)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;; https://emacs.stackexchange.com/questions/3244/how-to-make-mode-line-text-look-like-a-link-button-on-hover
(defun mode-line-initialize ()
  (setq-default mode-line-format
                '("%e"
                  (:eval
                    (let* (
                      ;; Mode line at left position.
                      (left-content (concat
                        (face (mode-line--buffer-name))
                        (face (format-mode-line "%l:%c"))))

                      ;; Mode line at right position.
                      (right-content (concat
                        (face (mode-line--evil-state))
                        (face (format-mode-line mode-name))
                        (face (mode-line--version-control))))

                      ;; Mode line at center position.
                      (center-fill
                        (mode-line--fill (+ (length right-content) 1))))

                      ;; Concat contents
                      (concat left-content center-fill right-content)))))

  (setq-default mode-line-format
    (cons (propertize "\u200b" 'display '((raise -0.5) (height 2))) mode-line-format)))
