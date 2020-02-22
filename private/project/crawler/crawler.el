;;; crawler.el --- Crawler Project File

;; Copyright (c) 2019-2019 Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: crawler project

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

;;;###autoload
(defvar crawler-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\pl" 'crawler-test-list)
    (define-key keymap "\C-c\pd" 'crawler-test-detail)
    keymap))

;;; Internal project functions.

(defun crawler-get-data-candidates ()
  "Return org files located in data directory."
  (let* ((list (directory-files "./data")))
    (dolist (f list)
      (when (not (string-match ".org$" f))
        (setq list (remove f list))))
    list))

(defun crawler-make (maker)
  "Find a source and execute a shell command for testing."
  (let* ((default-directory (projectile-project-root))
         (path (substring buffer-file-name (length default-directory)))
         (path-split (split-string path "\/"))
         (source path))

    (when (not (string-equal (nth 0 path-split) "data"))
      (ivy-read "Test Source: "
        (crawler-get-data-candidates)
        :require-match t
        :action (lambda (target)
                  (setq source (concat "data/" target)))))

    (async-shell-command (concat "make " maker " target=" source))))

;;; External project functions.

(defun crawler-test-list ()
  "Execute shell command 'make test_list'"
  (interactive)
  (crawler-make "test_list"))

(defun crawler-test-detail ()
  "Execute shell command 'make test_detail'"
  (interactive)
  (crawler-make "test_detail"))

;;; Define minor mode.

;;;###autoload
(define-minor-mode crawler-mode
  "A minor-mode to help to manage crawler configuration."
  nil " crawler" crawler-mode-map)

(provide 'crawler)
