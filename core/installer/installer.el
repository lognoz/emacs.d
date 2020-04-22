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

;;; Contextual installer constant.

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

(defconst template-evil-keybinding
  (template-content (concat embla-template-directory "evil-keybinding")))

(defconst template-define-key
  (template-content (concat embla-template-directory "define-key")))

(defconst template-autoload-statement
  (template-content (concat embla-template-directory "autoload-statement")))

(defconst auto-install-components-alist
  ;; Extension              Word syntax   Require   Mode
  '(("\\.js\\'"             '("-" "_")    nil       js2-mode)
    ("\\.scss\\'"           '("-" "_")    nil       scss-mode)
    ("\\.pp\\'"             '("-" "_")    nil       puppet-mode)
    ("PKGBUILD\\'"          '("-" "_")    nil       pkgbuild-mode)
    ("Dockerfile\\'"        '("-" "_")    nil       dockerfile-mode)
    ("\\.yml\\'"            '("-" "_")    nil       yaml-mode)
    ("\\.json\\'"           '("-" "_")    nil       json-mode)
    ("\\.rkt\\'"            '("-")        nil       racket-mode)
    ("\\.md\\'"             '("-")        nil       markdown-mode)
    ("\\.clj\\'"            '("-")        nil       clojure-mode)
    ("\\.tex\\'"            '("\\")       t         latex-mode)
    ("\\.editorconfig\\'"   '("-" "_")    t         editorconfig-conf-mode)))

;;; Contextual installer variables.

(defvar keybinding-component nil
  "List of keybinding define in component.")

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

(defun define-keybinding-state-normalize (args)
  "Return normalized `define-keybinding' states by allowed variables."
  (let ((plist-grouped) (plist) (definition) (is-definition))
    (while args
      (let ((arg (car args)))
        (setq is-definition nil)
        (when (member arg '(:normal :visual :define))
          (when plist
            (add-to-list 'plist-grouped `(,definition ,plist) t))
          (setq plist nil)
          (setq definition arg)
          (setq is-definition t))
        (when (and (not is-definition) definition)
          (setq plist
            (append plist (list arg)))))
      (setq args (cdr args)))
    (when plist
      (add-to-list 'plist-grouped `(,definition ,plist) t))
    plist-grouped))

(defun define-keybinding (&rest args)
  "Return normalized arguments."
  (let ((mode (plist-get args :mode))
        (formated-args (define-keybinding-state-normalize args)))
    (setq formated-args (append `((:mode ,mode)) formated-args))
    ;; Push value into `keybinding-component' variable.
    (add-to-list 'keybinding-component formated-args)
    formated-args))

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
  (package-set-archives))
  ;;(package-refresh-contents))

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
         (function-content))
    ;; Put these variables to nil to know what package is install
    ;; and keybinding is define inside this component.
    (setq embla-component-packages nil
          keybinding-component nil)
    ;; Install packages and append content of autoload and config.
    (load-files path '("package" "keybinding" "variable" "autoload" "config")
      (unless (or (equal f "package")
                  (equal f "keybinding")
                  (equal f "variable"))
        (push (file-content-without-header (concat path f ".el"))
              file-content)))
    ;; Build word syntax variable.
    (push (variable-word-syntax-in-module module component)
          function-content)
    ;; Build filename patterns for the module.
    (push (filename-pattern-in-module module component)
          file-content)
    ;; Build keybindings statement.
    (dolist (keybinding keybinding-component)
      (let ((mode (keybinding-extract-variable-content keybinding :mode)))
        (if (equal mode 'embla-mode-map)
          (push (replace-in-string template-autoload-statement
                  '((cons "__content__" (keybinding-in-module keybinding mode))))
                file-content)
          (push (keybinding-in-module keybinding mode) function-content))))
    ;; Loop into packages installed to add hooks and create init
    ;; function caller.
    (dolist (dependency embla-component-packages)
      (when-function-exists (concat module "-init-" dependency)
        (push (format "(%s)" func) function-content)))
    ;; Append and replace variable in file.
    (append-to-file file-to-write file-content)
    (replace-in-file file-to-write
      '((cons "__hook__" (hook-in-module module component))
        (cons "__module__" module)
        (cons "__component__" component)
        (cons "__content__" (mapconcat #'identity function-content "\n"))))))

(defun keybinding-extract-variable-content (keybindings key)
  "Return list of keybinginds."
  (when (assoc key keybindings)
    (car (cdr (assoc key keybindings)))))

(defun keybinding-build-global-content (keybindings mode)
  "Return list keybinding define into define-key statement."
  (setq keybindings (keybinding-extract-variable-content keybindings :define))
  (when keybindings
    (let ((content))
      (while keybindings
        (push
          (replace-in-string template-define-key
            '((cons "__key__" (prin1-to-string (car keybindings)))
              (cons "__function__" (prin1-to-string (cadr keybindings)))
              (cons "__mode__" (prin1-to-string mode))))
            content)
        (setq keybindings (cddr keybindings)))
      content)))

(defun keybinding-build-evil-content (keybindings state mode)
  "Return keybinding content by evil state."
  (setq keybindings (keybinding-extract-variable-content keybindings state))
  (when keybindings
    (replace-in-string template-evil-keybinding
      '((cons "__state__" (substring (prin1-to-string state) 1))
        (cons "__variable__" (prin1-to-string keybindings))
        (cons "__mode__" (prin1-to-string mode))))))

(defun keybinding-in-module (keybinding mode)
  "Return merged content builded by module keybinding."
  (let ((content))
    (setq content
      (append (keybinding-build-global-content keybinding mode)))
    (dolist (state '(:normal :visual))
      (push (keybinding-build-evil-content keybinding state mode)
        content))
    (mapconcat #'identity content "\n")))

(defun variable-word-syntax-in-module (module component)
  "Return content builded by module word syntax variable."
  (let* ((variable (concat module "-" component "-word-syntax"))
         (word-syntax (intern variable))
         (content))
    (when (boundp word-syntax)
      (setq word-syntax (symbol-value word-syntax))
      (setq content (format "(define-word-syntax '%s)" (prin1-to-string word-syntax))))
    content))

(defun filename-pattern-in-module (module component)
  "Return autoload filename pattern statements by variable."
  (let* ((extensions (intern (concat module "-" component "-filename-pattern")))
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
  (let* ((variable (concat module "-" component "-hook"))
         (reference (intern variable))
         (content))
    (when (boundp reference)
      (dolist (mode (symbol-value reference))
        (push (format template-hook-statement (symbol-name mode))
              content)))
    (if content
      (mapconcat #'identity content "\n")
      (replace-in-string template-autoload-statement
        '((cons "__content__" (format "(component-%s)" variable)))))))

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
  ;; Create component files.
  (dolist (directory (directories-list embla-component-directory))
    (let ((component (directory-name directory)))
      (make-component-files directory component)))
  ;; Scan core, component and project directories to create the
  ;; autoload file.
  (create-autoload-file))

(provide 'installer)
