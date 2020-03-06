;;; core-installer.el --- Core Installer File

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Keywords: core installer

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

(defvar template-component-hook-function
  (template-content
    (concat embla-core-directory "template/component-hook-function")))

(defvar template-component-hook-statement
  (template-content
    (concat embla-core-directory "template/component-hook-statement")))

(defvar template-simple-hook-statement
  (template-content
    (concat embla-core-directory "template/simple-hook-statement")))

(defvar component-autoload-file-content nil
  "The content of autoload component file.")

(defvar component-content nil
  "The content of component file.")

;;; Internal core functions.

(defun directory-name (path)
  ""
  (file-name-nondirectory
   (directory-file-name
     (file-name-directory path))))

(defun directories (path)
  ""
  (let ((directories))
    (dolist (f (directory-files path))
      (let ((path (concat path f)))
        (when (and (file-directory-p path)
                    (not (equal f "."))
                    (not (equal f "..")))
          (push (file-name-as-directory path) directories))))
    directories))

(defun delete-documentation-header ()
  "This function is used to delete documentation header"
  (let ((st (line-beginning-position)))
    (search-forward ";;; Code:")
    (delete-region st (point)))
  (delete-blank-lines))

(defun create-component-file ()
  "This function scan component directory to extract all autoload
files and merge it into one file."
  ;; Add the provide statement as last line of the content.
  (push "(provide 'embla-component)" component-autoload-file-content)
  ;; Extract all autoload.el in component directories to push it to
  ;; `component-autoload-file-content' variable.
  (dolist (path (directory-files-recursively (concat embla-component-directory "/startup/") ""))
    (when (string-equal (file-name-base path) "autoload")
      (push (with-temp-buffer
              (insert-file-contents path)
              (delete-documentation-header)
              (buffer-string))
        component-autoload-file-content)))
  ;; Add extracted content to file.
  (write-region
    (mapconcat #'identity component-autoload-file-content "\n")
    nil embla-component-file))

(defun create-autoload-file (&optional force)
  "This function parse magic comments locate in core and project
directories and append it to autoload file locate. It will helps to
optimize Embla."
  (let ((generated-autoload-file embla-autoload-file))
    ;; Clear content in autoload file.
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert "")
      (save-buffer))
    ;; Update autoload with embla component file generated earlier.
    (dolist (path (directory-files-recursively embla-build-directory ""))
      (update-file-autoloads path t generated-autoload-file))
    ;; Update autoload with core directory.
    (dolist (path (directory-files-recursively embla-core-directory ""))
      (update-file-autoloads path t generated-autoload-file))
    ;; Update autoload with project directory.
    (dolist (path (directory-files-recursively embla-project-directory ""))
      (update-file-autoloads path t generated-autoload-file))))

(defun refresh-package-repositories ()
  "This function is used to set and refresh package repositories."
  (message "Download descriptions of configured ELPA packages")
  (setq embla-package-initialized t)
  (package-set-archives)
  (package-refresh-contents))

(defun build-component-files (directory component)
  (fetch-content directory
    (let ((mode (concat module "-" component "-mode")))
      (build-component-file
        path module component mode))))

(defun file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (delete-documentation-header)
    (buffer-string)))

(defun build-component-file (path module component mode)
  ;; Remove file content because it cause problem when user rebuild
  ;; it on the same session.
  (setq component-content nil)
  ;; Put this variable to nil to know what package is install inside
  ;; this component.
  (setq embla-component-packages nil)
  ;; Add the provide statement as last line of the content.
  (push (format "(provide 'embla-%s-%s-component)" module component)
        component-content)
  ;; Add template hook function.
  (push template-component-hook-function component-content)
  ;; Load component files.
  (dolist (f '("package" "autoload" "config"))
    (when (file-exists-p (concat path f ".el"))
      (load (concat path f) nil 'nomessage)
      (unless (equal f "package")
        (push (file-contents (concat path f ".el")) component-content))))
  ;; Build hooks for the module.
  (push (build-hooks-in-module module component)
        component-content)
  ;; Loop into packages installed to add hooks.
  (dolist (dependency embla-component-packages)
    (when-function-exists (concat module "-hook-" dependency)
      (push (format template-simple-hook-statement (concat dependency "-hook") func)
            component-content)))
  ;; Define path by variables.
  (setq path
    (format "%sembla-%s-%s-component.el"
      embla-build-directory module component))
  ;; Add extracted content to file.
  (write-region
    (mapconcat #'identity component-content "\n")
    nil path)
  ;; Loop into packages installed in component to create init
  ;; function caller.
  (setq component-content nil)
  (dolist (dependency embla-component-packages)
    (when-function-exists (concat module "-init-" dependency)
      (push (format "(%s)" func) component-content)))
  ;; Replace variables.
  (replace-in-file path
    '((cons "__module__" module)
      (cons "__component__" component)
      (cons "__content__" (mapconcat #'identity component-content "\n")))))

(defun build-hooks-in-module (module component)
  (let* ((variable (concat module "-" component "-loader-hooks"))
         (reference (intern variable))
         (content))
    (when (boundp reference)
      (dolist (mode (symbol-value reference))
        (push
          (format template-component-hook-statement (symbol-name mode))
          content))
      (mapconcat #'identity content "\n"))))

(defun build-embla-startup-file ()
  ;; Remove file content because it cause problem when user rebuild
  ;; it on the same session.
  (setq component-content nil)
  ;; Add the provide statement as last line of the content.
  (push "(provide 'embla-startup)" component-content)
  ;; Fetch the startup directory locate in `embla-component-directory'.
  (fetch-content (concat embla-component-directory "startup/")
    ;; Put this variable to nil to know what package is install inside
    ;; this component.
    (setq embla-component-packages nil)
    ;; Load component files.
    (dolist (f '("package" "config"))
      (when (file-exists-p (concat path f ".el"))
        (load (concat path f) nil 'nomessage)))
    ;; Loop into packages installed in component to create init
    ;; function caller.
    (dolist (dependency embla-component-packages)
      (when-function-exists (concat module "-init-" dependency)
        (push (format "(%s)" func) component-content)))
    ;; Build content file and remove documentation header.
    (push (with-temp-buffer
            (insert-file-contents (concat path "/config.el"))
            (delete-documentation-header)
            (buffer-string))
      component-content)
    ;; Add extracted content to file.
    (write-region
      (mapconcat #'identity component-content "\n")
      nil embla-startup-file)))

;;; External core functions.

(defun embla-install-program ()
  "This function is the main installer function. It create autoload
file, refresh package repositories and build Embla."
  ;; Recreate build directory.
  (delete-directory embla-build-directory t)
  (make-directory embla-build-directory)
  ;; Create component file that contains the autoloads functions in
  ;; component directory.
  (create-component-file)
  ;; Refresh package repositories.
  (refresh-package-repositories)
  ;; Install Embla theme.
  (require-package 'atom-one-dark-theme)
  ;; Build file that's load on Emacs startup.
  (build-embla-startup-file)
  ;; Build component files.
  (dolist (directory (directories embla-component-directory))
    (let ((component (directory-name directory)))
      (unless (equal component "startup")
        (build-component-files directory component))))
  ;; Scan core, component and project directories to create the
  ;; autoload file.
  (create-autoload-file))

(provide 'core-installer)
