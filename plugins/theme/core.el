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
(require 'lib/pkg/core)

(defun theme/initialize ()
  "Initilize Noteditor theme plugin"
  (pkg/use dracula-theme)
  (load-theme 'dracula t)
  (set-face-attribute 'default nil :height 120)
  (theme/welcome-message)
  )

(defun theme/welcome-message ()
  (add-hook 'emacs-startup-hook 'my-startup-fcn)
(defun my-startup-fcn ()
  (let ((my-buffer (get-buffer-create "my-buffer")))
    (with-current-buffer my-buffer
      (insert "Welcome to Noteditor\nFor more information visit https://github.com/yottanami/noteditor"))
    (switch-to-buffer my-buffer)))
)

(provide 'plugins/theme/core)
;;; core.el ends here
