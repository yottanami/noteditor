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

;; Abort loudly on load errors only when explicitly debugging
;; (set NOTEDITOR_DEBUG=true).  A normal launch stays quiet.
(setq debug-on-error (string= (getenv "NOTEDITOR_DEBUG") "true"))


(defvar noteditor-home (getenv "NOTEDITOR_HOME")
  "The pass to noteditor-home.")
(add-to-list 'load-path noteditor-home)


;;(setq custom-file (format "%s/.noteditor.custom.el" (getenv "HOME")))
(setq user-emacs-directory "~/.noteditor/emacs.d")

;; Load the user override file.  Under Nix the bundled copy lives in the
;; read-only store, so prefer a user-writable location.  Earlier entries
;; win; the bundled stub is the final fallback.
(let ((user-files
       (list (expand-file-name "noteditor/noteditor-user.el"
                               (or (getenv "XDG_CONFIG_HOME")
                                   (expand-file-name "~/.config")))
             (expand-file-name "~/.noteditor/noteditor-user.el")
             (and noteditor-home
                  (expand-file-name "noteditor-user.el" noteditor-home)))))
  (catch 'loaded
    (dolist (f (delq nil user-files))
      (when (file-exists-p f)
        (load f nil 'nomessage)
        (throw 'loaded f)))))

(require 'core/utils)

(require 'lib/pkg/core)
(pkg/initialize)


(let ((wm-mode (getenv "NOTEDITOR_WM")))
  (if (string= wm-mode "true")
    (load-plugin "wm")
    (progn
      (load-plugin "editor")
      (load-plugin "devel"))
  )
)
(load-plugin "theme")
(load-plugin "org")

(provide 'noteditor)
;;; noteditor-config.el ends here
