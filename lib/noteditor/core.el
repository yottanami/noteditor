;;; Noteditor --- Based on FG42 -*- lexical-binding: t; -*-
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
;;; Code:

(defun noteditor/initialize ()
  "Initialize the Noteditor."
  (require 'pkg/core)
  (pkg/initialize)

  (when (file-exists-p user-init-file)
    (load user-init-file)))

(defvar noteditor-home (getenv "NOTEDITOR_HOME")
  "The pass to fg42-home.")

(provide 'noteditor/core)
;;; core.el ends here