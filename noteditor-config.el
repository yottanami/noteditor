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
(setq debug-on-error t)


(defvar noteditor-home (getenv "NOTEDITOR_HOME")
  "The pass to noteditor-home.")
(add-to-list 'load-path noteditor-home)


;;(setq custom-file (format "%s/.noteditor.custom.el" (getenv "HOME")))
(setq user-emacs-directory "~/.noteditor/emacs.d")

(setq user-init-file "noteditor-user.el")

(when (file-exists-p user-init-file)
  (load user-init-file))

(require 'core/utils)

(require 'lib/pkg/core)
(pkg/initialize)


(let ((wm-mode (getenv "NOTEDITOR_WM")))
  (when (string= wm-mode "true")
    (load-plugin "wm")
    )
  )
(load-plugin "theme")
(load-plugin "org")

(provide 'noteditor)
;;; noteditor-config.el ends here
