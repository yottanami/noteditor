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


(require 'plugins/wm)

(defcube noteditor/editor
  "Loads all plugins"
  :title "Editor plugin"
  :flag-default t
  :flag noteditor-editor-plugin
  :init-hook (lambda (params)
               (mapc
                (lambda (plugin)
                  (when (not (string= (symbol-name plugin)
                                      "noteditor/editor"))
                    (let ((cube-params (plist-get
                                        params
                                        (intern (concat ":" (symbol-name cube))))))
                      (eval `(funcall #',cube ,@cube-params)))))
                fg42/available-cubes)))



(provide 'cubes/fg42)
;;; fg42.el ends here
