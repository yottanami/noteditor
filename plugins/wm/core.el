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

(defun wm/initialize ()
  "Initilize EXWM window manager with the given PARAMS."
  (pkg/use exwm)
  (interactive)
  (message "WM plugin loaded")
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (require 'exwm-randr)
  (exwm-config-default)
  (wm/exwm-change-screen-hook)
  (exwm-randr-enable)
  )

(defun wm/exwm-change-screen-hook ()
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--scale" "0.5x0.5")
        (call-process
         "xrandr" nil nil nil
	 "--output" default-output "--scale" "0.5x0.5"
         "--output" (match-string 1) "--primary" "--above" default-output)
        (setq exwm-randr-workspace-output-plist (list 6 default-output))))))

(provide 'plugins/wm/core)
;;; core.el ends here
