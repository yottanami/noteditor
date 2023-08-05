;;; Noteditor --- Based on FG42 -*- lexical-binding: t; -*-
;;
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'noteditor/core)


(defvar noteditor/available-flags nil
  "A list of defined flags.

Only use \\[defflag] to add a new flag to this list")


(defcustom noteditor/flags nil
  "A set of flags to mark the functionalities that expected from NOTEDITOR.
Flags are defined using the \\[defflag] through out the source code.

To see a list of available flags use \\[noteditor/show-all-flags] and to see
the documentation of each flag simply use \\[describe-flag]."
  :group 'noteditor
  :package-version '(NOTEDITOR . "3.x")
  :type '(symbol)
  :tag "NOTEDITOR Flags")


(defmacro use-flags (flags)
  "Set the given FLAGS to activate their functionalities in NOTEDITOR."
  `(progn
     (setq noteditor/flags ,flags)
     t))


(defun noteditor/-merge-flags (flags-set &rest new-flags)
  "Merge the given NEW-FLAGS into the FLAGS-SET and return the result."
  (cl-remove-duplicates
   (seq-reduce
    (lambda (result flag)
      (let ((flag-str (symbol-name flag)))
        (if (string-prefix-p "-" flag-str)
            (let ((actual-flag (intern (substring flag-str 1))))
              (if (member actual-flag result)
                  (remove actual-flag result)
                result))
          ;; We don't want to check for duplicates here since we remove them
          ;; later
          (cons flag result))))
    new-flags
    flags-set)))


(defmacro noteditor/merge-flags (flags-set &rest new-flags)
    "Merge the given NEW-FLAGS into the FLAGS-SET and return the result.

If any flag name in NEW-FLAGS list starts with `-' (a dash) it implies that
that functionality has to be disabled and removed from FLAGS-SET.  For example,
`-lsp' implies that we DO NOT want to have `lsp' flag enabled and it should not
exist in the return value.

For example, `(noteditor/merge-flags (list f1 f2 f3) f4 -f2)' will return `(f1 f3 f4)'"
    `(noteditor/-merge-flags ,flags-set ,@(mapcar (lambda (x) `',x) new-flags)))


(defmacro noteditor/merge-with-default-flags (&rest new-flags)
  "Merge the given list of NEW-FLAGS with default flags of NOTEDITOR and return the new set."
  `(noteditor/merge-flags noteditor/flags ,@new-flags))


(defmacro defflag (flag-name docstring &optional default-value)
  "Define a new flag FLAG-NAME with the given DOCSTRING.
If the DEFAULT-VALUE is a non nil value, then the flag will be
added to the active flags list."
  (declare (doc-string 2) (indent defun))
  (let ((var-name (intern (format "noteditor/-flag-%s" (symbol-name flag-name))))
        (set-default (when default-value
                       `((add-to-list 'noteditor/flags ',flag-name)))))

    `(if (boundp ',var-name)
         (warn (format "Flag name `%s' already defined" ,(format "%s" flag-name)))
       (progn
         (defvar ,var-name t)
         (add-to-list 'noteditor/available-flags ',flag-name)
         ,@set-default))))


(defmacro when-flag (flag &rest body)
  "Evaluate the BODY only if the given FLAG is active."
  (declare (indent defun))
  ;; The `plguin-local-flags' variable here is going to be
  ;; defined in plguins to hold the local flags for each plguin
  (if (not (and (boundp 'plguin-local-flags)
                (member flag plguin-local-flags)))
      `(progn ,@body)
    `(when (member ',flag noteditor/flags)
       ,@body)))


(defmacro if-flag (flag then else)
  "Evaluate the THEN expr only if the given FLAG is active otherwise ELSE."
  (declare (indent defun))
  ;; The `plguin-local-flags' variable here is going to be
  ;; defined in plguins to hold the local flags for each plguin
  (if (and (boundp 'plguin-local-flags)
           (member flag plguin-local-flags))
      then
     `(if (member ',flag noteditor/flags)
         ,then
       ,else)))


(provide 'noteditor/flags)
;;; flags.el ends here
