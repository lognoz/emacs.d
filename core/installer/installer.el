;;; installer.el --- Installer File

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

(defconst template-auto-mode-alist
  (template-content (concat embla-template-directory "auto-mode-alist")))

(defconst template-auto-install-hook
  (template-content (concat embla-template-directory "auto-install-hook")))

(defconst template-hook-function
  (template-content (concat embla-template-directory "hook-function")))

(defconst template-hook-statement
  (template-content (concat embla-template-directory "hook-statement")))

(defconst template-simple-hook-statement
  (template-content (concat embla-template-directory "simple-hook-statement")))

(defconst auto-install-components-alist
  ;; Extension              Word syntax   Require   Mode
  '(("\\.js\\'"             '("-" "_")    nil       js2-mode)
    ("\\.scss\\'"           '("-" "_")    nil       scss-mode)
    ("\\.pp\\'"             '("-" "_")    nil       puppet-mode)
    ("PKGBUILD\\'"          '("-" "_")    nil       pkgbuild-mode)
    ("Dockerfile\\'"        '("-" "_")    nil       dockerfile-mode)
    ("\\.yml\\'"            '("-" "_")    nil       yaml-mode)
    ("\\.json\\'"           '("-" "_")    nil       json-mode)
    ("\\.tex\\'"            '("\\")       t         latex-mode)
    ("\\.editorconfig\\'"   '("-" "_")    t         editorconfig-conf-mode)))

;;; Internal installer functions.

(defmacro load-files (path files &rest body)
  "Load multiples files by reference path."
  `(dolist (f ,files)
    (when (file-exists-p (concat ,path f ".el"))
      (load (concat ,path f) nil 'nomessage)
      ,@body)))

(defun file-content-without-header (path)
  "Return file content witout documentation header."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((st (line-beginning-position)))
      (search-forward ";;; Code:")
      (delete-region st (point)))
    (delete-blank-lines)
    (buffer-string)))

(defun append-to-file (path content)
  "Write content into the end of a file."
  (write-region
    (mapconcat #'identity content "\n") nil path))

(defun auto-install-content (package)
  "Return content of package hooks and auto-mode-alist. It's
mainly used with mapconcat."
  (let ((extension (prin1-to-string (car package)))
        (syntax (prin1-to-string (cadr package)))
        (built-in (cadr (cdr package)))
        (mode (cadr (cdr (cdr package)))))
    (unless built-in
      (require-package mode))
    (setq mode (symbol-name mode))
    (concat
      (replace-in-string template-auto-install-hook
        '((cons "__mode__" mode)
          (cons "__syntax__" syntax)))
      (replace-in-string template-auto-mode-alist
        '((cons "__mode__" mode)
          (cons "__extension__" extension))))))

(defun create-auto-install-file ()
  "Create a file that contains all auto-installed packages."
  (let ((path (concat embla-build-directory "component-auto-install.el")))
    (write-region
      (mapconcat #'auto-install-content auto-install-components-alist "\n")
      nil path)))

(defun refresh-package-repositories ()
  "Set archives and refresh package repositories."
  (message "Download descriptions of configured ELPA packages")
  (setq embla-package-initialized t)
  (package-set-archives)
  (package-refresh-contents))

(defun create-startup-autoload-file ()
  "Scan component directory to extract all autoload files and
merge it into one file."
  (let ((path-reference (concat embla-component-directory "/startup/"))
        (file-content '("(provide 'embla-component)")))
    ;; Extract all autoload.el in component directories to push it to
    ;; `component-autoload-file-content' variable.
    (dolist (path (directory-files-recursively path-reference ""))
      (when (string-equal (file-name-base path) "autoload")
        (push (file-content-without-header path)
              file-content)))
    (append-to-file embla-component-file file-content)))

(defun create-startup-component-file ()
  "Extract all content into subdirectories locate in startup
component to write an unique file that contains it."
  (let ((file-content '("(provide 'embla-startup)")))
    (fetch-content (concat embla-component-directory "startup/")
      ;; Put this variable to nil to know what package is install inside
      ;; this component.
      (setq embla-component-packages nil)
      ;; Loop into packages installed in component to create init
      ;; function caller.
      (load-files path '("package" "config"))
      (dolist (dependency embla-component-packages)
        (when-function-exists (concat module "-init-" dependency)
          (push (format "(%s)" func) file-content)))
      (push (file-content-without-header (concat path "/config.el"))
            file-content)
      (append-to-file embla-startup-file file-content))))

(defun create-autoload-file ()
  "This function parse magic comments locate in core and project
directories and append it to autoload file locate. It will helps to
optimize Embla."
  (let ((generated-autoload-file embla-autoload-file)
        (directories '(embla-build-directory embla-core-directory embla-project-directory)))
    ;; Clear content in autoload file.
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert "")
      (save-buffer))
    ;; Update autoload on build, core and project directories.
    (dolist (directory directories)
      (dolist (path (directory-files-recursively (symbol-value directory) ""))
        (unless (equal (directory-name path) "template")
          (update-file-autoloads path t generated-autoload-file))))))

(defun make-component-files (directory component)
  "Fetch directories to create component file."
  (fetch-content directory
    (let ((mode (concat module "-" component "-mode")))
      (make-component-file
        path module component mode))))

(defun make-component-file (path module component mode)
  "Create component file by given subdirectories arguments."
  (let* ((provider (format "(provide 'component-%s-%s)" component module))
         (file-to-write
           (format "%scomponent-%s-%s.el"
                   embla-build-directory component module))
         (file-content (list template-hook-function provider))
         (variable-init-content))
    ;; Install packages and append content of autoload and config.
    (load-files path '("package" "autoload" "config")
      (unless (equal f "package")
        (push (file-content-without-header (concat path f ".el"))
              file-content)))
    ;; Build hooks for the module.
    (push (hook-in-module module component)
          file-content)
    ;; Build filename patterns for the module.
    (push (filename-pattern-in-module module component)
          file-content)
    ;; Loop into packages installed to add hooks and create init
    ;; function caller.
    (dolist (dependency embla-component-packages)
      (when-function-exists (concat module "-init-" dependency)
        (push (format "(%s)" func) variable-init-content))
      (when-function-exists (concat module "-define-keybinding")
        (push (format "(%s)" func) variable-init-content))
      (when-function-exists (concat module "-hook-" dependency)
        (push (format template-simple-hook-statement (concat dependency "-hook") func)
              file-content)))
    (append-to-file file-to-write file-content)
    (replace-in-file file-to-write
      '((cons "__module__" module)
        (cons "__component__" component)
        (cons "__content__" (mapconcat #'identity variable-init-content "\n"))))))

(defun filename-pattern-in-module (module component)
  "Return autoload filename pattern statements by variable."
  (let* ((extensions (intern (concat module "-" component "-filename-patterns")))
         (mode (intern (concat module "-" component "-major-mode")))
         (content))
    (when (and (boundp extensions) (boundp mode))
      (dolist (extension (symbol-value extensions))
        (push (replace-in-string template-auto-mode-alist
                '((cons "__mode__" (prin1-to-string (symbol-value mode)))
                  (cons "__extension__" (prin1-to-string extension))))
          content)))
    (mapconcat #'identity content "\n")))

(defun hook-in-module (module component)
  "Return autoload hook statements by variable."
  (let* ((variable (concat module "-" component "-loader-hooks"))
         (reference (intern variable))
         (content))
    (when (boundp reference)
      (dolist (mode (symbol-value reference))
        (push (format template-hook-statement (symbol-name mode))
              content))
      (mapconcat #'identity content "\n"))))

;;; External installer functions.

(defun embla-install-program ()
  "This function is the main installer function. It create autoload
file, refresh package repositories and build Embla."
  ;; Recreate build directory.
  (delete-directory embla-build-directory t)
  (make-directory embla-build-directory)
  ;; Refresh package repositories.
  (refresh-package-repositories)
  ;; Install embla theme.
  (require-package 'atom-one-dark-theme)
  ;; Create component file with `auto-install-alist'.
  (create-auto-install-file)
  ;; Create component file that contains the autoloads functions in
  ;; component directory.
  (create-startup-autoload-file)
  ;; Create file that's load on Emacs startup.
  (create-startup-component-file)
  ;; Create component files.
  (dolist (directory (directories-list embla-component-directory))
    (let ((component (directory-name directory)))
      (unless (equal component "startup")
        (make-component-files directory component))))
  ;; Scan core, component and project directories to create the
  ;; autoload file.
  (create-autoload-file))

(provide 'installer)
