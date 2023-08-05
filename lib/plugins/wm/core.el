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

(require 'noteditor/flags)

(defun wm-randr ()
  "RandR support for wm."
  (when-wm
   (require 'exwm-randr)
   (setq exwm-randr-workspace-output-plist '(0 "HDMI-1"
                                             1 "HDMI-1"
                                             2 "HDMI-1"
                                             3 "HDMI-1"
                                             4 "HDMI-1"
                                             5 "HDMI-1"
                                             6 "eDP-1"
                                             7 "HDMI-1"
                                             8 "HDMI-1"
                                             9 "HDMI-1"))
   (add-hook 'exwm-randr-screen-change-hook
             (lambda ()
               (start-process-shell-command
                "xrandr" nil "xrandr --output HDMI-1 --above eDP-1 --mode 1920x1080")))
   (exwm-randr-enable)))


(defun noteditor/initialize-wm ()
  "Initilize EXWM window manager with the given PARAMS."
  (interactive)

  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-systemtray)

  (exwm-config-ido)

  ;; Set the initial number of workspaces (they can also be created later).
  (setq exwm-workspace-number 10)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
  ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
  ;; are run when a new X window class name or title is available.  Here's
  ;; some advice on this topic:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of
  ;;    all windows are probably the same.  Using window titles for them makes
  ;;   more sense.
  ;; In the following example, we use class names for all windows except for
  ;; Java applications and GIMP.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

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
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-d] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))))
  ;; To add a key binding only available in line-mode, simply define it in
  ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  (push ?\C-c exwm-input-prefix-keys)

  ;; The following example demonstrates how to use simulation keys to mimic
  ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
  ;; and DEST is what EXWM actually sends to application.  Note that both SRC
  ;; and DEST should be key sequences (vector or string).
  (setq exwm-input-simulation-keys
        `(
          ;; movement
          (,(kbd "C-b") . left)
          (,(kbd "M-b") . ,(kbd "C-<left>"))
          (,(kbd "C-f") . right)
          (,(kbd "M-f") . ,(kbd "C-<right>"))
          (,(kbd "C-p") . up)
          (,(kbd "C-n") . down)
          (,(kbd "C-a") . home)
          (,(kbd "C-e") . end)
          (,(kbd "M-v") . prior)
          (,(kbd "C-v") . next)
          (,(kbd "C-d") . delete)
          ;;(,(kbs "C-k") . [S-end delete])
          ;; navigation
          (,(kbd "C-c b") . ,(kbd "M-<left>"))
          (,(kbd "C-c f") . ,(kbd "M-<right>"))
          (,(kbd "C-c w") . ,(kbd "C-w"))
          (,(kbd "C-w") . ,(kbd "C-x"))
          (,(kbd "M-w") . ,(kbd "C-c"))
          (,(kbd "C-y") . ,(kbd "C-v"))
          ;; search
          (,(kbd "C-s") . ,(kbd "C-f"))))

  ;; You can hide the minibuffer and echo area when they're not used, by
  ;; uncommenting the following line.
  ;;(setq exwm-workspace-minibuffer-position 'bottom)

  ;; Do not forget to enable EXWM. It will start by itself when things are
  ;; ready.  You can put it _anywhere_ in your configuration.
  (exwm-enable)
  (exwm-systemtray-enable)
  (wm-randr)
  ;; (with-flag nlinum
  ;;            (add-hook 'exwm-mode-hook 'disable-nlinum))
  )


(provide 'plugins/wm/core)
;;; core.el ends here
