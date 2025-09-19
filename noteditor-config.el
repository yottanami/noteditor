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


(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))


;;(setq custom-file (format "%s/.noteditor.custom.el" (getenv "HOME")))

(require 'core/utils)
(require 'core/config)

(require 'lib/pkg/core)
(pkg/initialize)


(cond
 ((eq noteditor-mode 'wm) (load-plugin "wm"))
 (t (load-plugin "editor")))
(load-plugin "theme")
(load-plugin "org")

(provide 'noteditor)
;;; noteditor-config.el ends here
