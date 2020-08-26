;;; lisp/core-macros.el --- core macros -*- lexical-binding: t; -*-

;; Copyright (c) Marc-Antoine Loignon

;; Author: Marc-Antoine Loignon <developer@lognoz.org>
;; Homepage: https://github.com/lognoz/embla
;; Keywords: macros

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

(defmacro defer-loading-eval (events functions)
  "Eval `add-hook' and `advice-add' statements if it's a function.

This function expect to recives a list of EVENTS used as hooks
and a list of FUNCTIONS to trigger."
  (let ((fn `(lambda (&rest _)
               (mapcar #'funcall ',functions))))
    (dolist (on events)
      (eval
       (if (functionp on)
           `(advice-add ',on :before ,fn)
         `(add-hook ',on ,fn))))))

(defmacro defer-loading (&rest args)
  "Hook functions to anothers one or hooks variables.

  (defer-loading
     [:keyword [ARGS]]...)

:event     Functions or hooks variables used as trigger function.
:function  Functions that you want to trigger."
  (let ((functions)
        (targets)
        (is-event)
        (is-function))
    (dolist (key `(list ,@args))
      (cond
       ((eq key :event)
        (setq is-function nil
              is-event t))
       ((eq key :function)
        (setq is-function t
              is-event nil))
       (is-event (push key targets))
       (is-function (push key functions))))
    `(defer-loading-eval ,targets ,functions)))


(provide 'core-macros)

;;; core-macros.el ends here
