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
(defun inject-straight (args)
  "Inject `:straight t' to ARGS it the key was missing."
  (if (member :straight args)
      args
    (append args '(:straight t))))


(defun inject-defer (args)
  "Inject `:defer t' to ARGS it the key was missing."
  (if (member :defer args)
      args
    (append args '(:defer t))))


(defmacro pkg/use (pkg &rest details)
  "Install the given package DETAILS PKG via use-package and straight."
  (declare (indent defun))

  (if (and (listp details) (< 0 (length details)))
      (let ((params (inject-straight (inject-defer details))))
          (progn
            `(use-package ,pkg ,@params)))
    `(use-package ,pkg :straight t :defer t)))


(defmacro pkg/require (pkg)
  "Work like require but make sure that PKG is installed first."
  (let ((pkg-name (intern (symbol-name `,(cadr pkg)))))
    `(pkg/use ,pkg-name
       :init
       (require ,pkg))))

(provide 'pkg)
;;; pkg.el ends here
