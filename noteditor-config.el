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

(add-to-list 'load-path (concat (getenv "NOTEDITOR_HOME") "/lib"))

;; Prevent package.el to install anything at startup
(setq package-enable-at-startup nil)

(setq custom-file (format "%s/.noteditor.custom.el" (getenv "HOME")))
(setq user-emacs-directory "~/.noteditor/emacs.d")
(setq user-init-file
      (format "%s/.noteditor.el"
              (getenv "HOME")))


;; Load the custom ization file. In FG42 it is different than
;; the default `user-init-file'
(when (file-exists-p custom-file)
    (load custom-file))

(require 'noteditor/core)

(noteditor/initialize)


(provide 'noteditor)
;;; noteditor-config.el ends here
