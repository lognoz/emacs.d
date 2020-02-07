;;; core-mode-line.el --- Mode line Initialization File

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

(defun mode-line--version-control ()
  (when (stringp vc-mode)
    (format "%s%s"
      (char-to-string #xe0a0)
      (replace-regexp-in-string
        (format "^ %s[:-]" (vc-backend buffer-file-name)) " " vc-mode))))

(defun mode-line--buffer-name ()
  "Render a different buffer name if version control is active or not."
  (let ((text (buffer-name)) (face nil))
    (if buffer-file-name
      (let ((project-root (projectile-project-root)))
        (when project-root
          ;; Define text variable with buffer path with the version control location.
          (setq text (substring buffer-file-name (length project-root)))))
      ;; If it's an Emacs system buffer change font style to italic.
      (setq face 'italic))
    (propertize (concat " " text) 'face face)))

(defun mode-line--evil-state ()
  (cond
    ((eq evil-state 'insert) "Insert")
    ((eq evil-state 'visual) "Visual")
    ((eq evil-state 'replace) "Replace")
    (t "Normal")))

(defun mode-line--evil-macro ()
  (when evil-this-macro
     (propertize
       (concat "Recording @" (string evil-this-macro))
           'face '(:foreground "orange"))))

(defun mode-line--mode-name ()
  (propertize mode-name
              'face 'bold))

(defun mode-line--column-line ()
  (when (derived-mode-p 'prog-mode)
    (face (format-mode-line "%l,%c"))))

(defun face (text)
  (when (not (= (length text) 0))
    (concat " " text " ")))

(defun mode-line--fill (reserve)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;;;###autoload
(defun mode-line-initialize ()
  (setq-default
    mode-line-format
    '("%e"
      (:eval
        (let* (
          ;; Mode line at left position.
          (left-content (concat
            (face (mode-line--buffer-name))
            (face (mode-line--column-line))
            (face (mode-line--mode-name))))

          ;; Mode line at right position.
          (right-content (concat
            ;;(face (mode-line--evil-macro))
            (face (mode-line--evil-state))
            (face (mode-line--version-control))))

          ;; Mode line at center position.
          (center-fill
            (mode-line--fill (+ (length right-content) 1))))

          ;; Concat contents
          (concat left-content center-fill right-content)))))

  (setq-default mode-line-format
    (cons (propertize "\u200b" 'display '((raise -0.3) (height 1.8))) mode-line-format)))

(provide 'core-mode-line)
