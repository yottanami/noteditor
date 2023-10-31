;;; Noteditor --- Based on NOTEDITOR -*- lexical-binding: t; -*-
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

 ;; (require 'noteditor/flags)
 ;; (require 'plugins/editor)
;; (require 'core/utils)

;; (message "Default user file loaded ...")
;; (use-flags
;;  (wm))


;; ;TODO: Move this blog to a macro or something ===========
;; (when-wm
;;  (setq global-font-size 8)
;;  ;; Change the resolution and monitors to your need
;;  (defvar monitors
;;    '(:hdmi-only
;;      ("--output DP-3-1-5 --primary"
;;       "--output eDP-1 --off")
;;      :hdmi-main
;;      ("--output DP-3-1-5 --primary --mode 2560x1080"
;;       "--output eDP-1 --scale 0.5x0.5 --below DP-3-1-5")
;;      :edp-only
;;      ("--output eDP-1 --scale 0.5x0.5"
;;       "--output HDMI-1 --off --output DP-3-1-5 --off")))

;;  (require 'seq)
;;  (defun monitor-profiles ()
;;    (mapcar
;;     #'car
;;     (seq-partition monitors 2)))

;;  (defun monitor (mon)
;;    (interactive
;;     (list (completing-read
;;            "Monitor Profole: "
;;            (monitor-profiles))))

;;    (let ((cmd (mapconcat (lambda (x) (format "xrandr %s" x))
;;                          (plist-get monitors (intern (format "%s" mon)))
;;                          " && ")))
;;      (message "Setting monitor profile: %s" cmd)
;;      (async-shell-command cmd "*xrandr*")))

;;  (monitor :hdmi-main)

;;  (use-flags
;;   (wm)))


;; (when (file-exists-p "~/.noteditor-user.el")
;;   (load "~/.noteditor-user.el"))

(provide 'noteditor-user) ;;;
;;; noteditor-user.el ends here
