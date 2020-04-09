;;; functionality-project.el --- Project Function File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: functionality project

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

(defun create-directory-local-variable-file (source)
  "Create .dir-locals.el file on project root."
  (interactive "sProject source: ")
  (let* ((project-root (projectile-project-root))
         (path (concat project-root ".dir-locals.el"))
         (template-dir-locals
           (template-content (concat embla-template-directory "dir-locals"))))
    (write-region
      (replace-in-string template-dir-locals
        '((cons "__source__" source)))
      nil path)))

(defun directories-attributes (directory)
  "Return list of directories and its attributes like last
modification, user permissions, etc."
  (let ((directories))
    (dolist (attributes (directory-files-and-attributes directory))
      (let ((path (car attributes))
            (is-directory (cadr attributes)))
        (unless (or (equal path ".")
                    (equal path "..")
                    (not is-directory))
          (setf (cadr attributes) (concat directory path "/"))
          (add-to-list 'directories attributes))))
    directories))

(defun find-project-candidates ()
  "Return a list candidates sorted by last modification for
`find-project' function."
  (let ((directories))
    (dolist (directory project-directories)
      (setq directories (append directories (directories-attributes directory))))
    (mapcar
      (lambda (directory)
        (cons (car directory) (cadr directory)))
      (sort directories
            #'(lambda (x y)
                (time-less-p (nth 6 y) (nth 6 x)))))))

;;;###autoload
(defun browse-project-source ()
  "Browse project source path. Variable `project-source' is
normally define into .dir-locals.el at the base of the project."
  (interactive)
  (unless (projectile-project-root)
    (error "This function can only be exectued in project."))
  (when (and (boundp 'project-source)
             (equal (type-of project-source) 'cons))
    (if (equal (length project-source) 1)
      (browse-url (car project-source))
      (ivy-read "Browse source: " project-source
        :require-match t
        :action (lambda (target)
          (browse-url target))))
    (message "Open url on browser")))

;;;###autoload
(defun find-directory-local-variable-file ()
  "Find and open project .dir-locals.el. If it doesn't exist, a
prompt will ask if the user want to create it."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (path (concat project-root ".dir-locals.el")))
    (unless project-root
      (error "This function can only be exectued in project."))
    (if (file-exists-p path)
      (find-file path)
      (when (yes-or-no-p "File .dir-locals.el couldn't been found. Do you want to create it? ")
        (call-interactively 'create-directory-local-variable-file)
        (find-file path)))))

;;;###autoload
(defun find-project ()
  "Find projects under directories define in `project-directories'
variables. Return candidates is sorting by last modification."
  (interactive)
  (unless project-directory
    (error "No project-directories variable is defined."))
  (let ((candidates (find-project-candidates)))
    (ivy-read "Open project: " candidates
      :require-match t
      :action (lambda (target)
        (dired (cdr target))))))
