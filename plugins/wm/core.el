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
          (call-process "xrandr" nil nil nil "--output" default-output );;"--scale" "0.5x0.5")
        (call-process
         "xrandr" nil nil nil
	 "--output" default-output ;;"--scale" "0.5x0.5"
         "--output" (match-string 1) "--primary" "--above" default-output)
        (setq exwm-randr-workspace-monitor-plist (list 6 default-output)))))
  )


 ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ([?\s-g] . keyboard-quit)
          ([8388640] . other-window)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-d" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
	  
          ;; Bind "s-l" to "screen lock"
          ([?\s-l] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/i3lock")))

	  ;; Bind "s-s" to "screenshot".
          ([?\s-s] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/gnome-screenshot")))

	  ;; Bind "s-m" to "media player".
          ([?\s-m] . (lambda ()
                     (interactive)
                     (start-process "" nil "/home/yottanami/bin/Plexamp-4.9.5.AppImage")))
	  
	  ;; Bind "s-b" to "browser".
          ([?\s-b] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/brave-browser")))
	  
	  ;; Bind "s-x" to "terminal".
          ([?\s-x] . (lambda ()
                     (interactive)
                     (start-process "" nil "/usr/bin/alacritty")))

	  ;; Bind "s-t" to "tab-bar-mode".
	  ([?\s-t] . tab-bar-mode)
	  
	  ))


;;;(global-set-key (kbd "s-t") 'tab-bar-mode)
;;; set s-r key binding to run tab-bar-mode



(provide 'plugins/wm/core)
;;; core.el ends here
