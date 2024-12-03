;;; plugins/wm/core.el --- EXWM Window Manager Configuration -*- lexical-binding: t; -*-

;; Author: Your Name <your.email@example.com>
;; URL: https://your-repo-url.example.com
;; License: GPL-3+

;;; Commentary:

;; This file contains the configuration for EXWM (Emacs X Window Manager),
;; including dynamic handling of display configurations when connecting or
;; disconnecting external monitors.

;;; Code:

(require 'lib/pkg/core)
(require 'cl-lib)

;;; Variables

(defvar wm/external-monitor-position "--above"
  "Position of the external monitor relative to the internal display.
Possible values include '--above', '--below', '--left-of', '--right-of'.")

;;; Initialization

(defun wm/initialize ()
  "Initialize EXWM window manager."
  (interactive)  
  (pkg/use exwm)
  (require 'exwm)
  (require 'exwm-config)
  (require 'exwm-systemtray)
  (require 'exwm-randr)
  ;; Configure EXWM
  (exwm-config-default)
  ;; Set up screen change hook
  (add-hook 'exwm-randr-screen-change-hook #'wm/update-displays)
  ;; Enable RandR support
  (exwm-randr-enable)
  ;; Enable system tray
  (exwm-systemtray-enable)
  ;; Set global keybindings
  (wm/setup-global-keybindings)
  (start-process-shell-command "dunst" nil "dunst")
  )

;;; Helper Functions

(defun wm/get-connected-outputs ()
  "Get the list of connected outputs, separating internal and external monitors.
Returns a cons cell (INTERNAL . EXTERNALS), where INTERNAL is the internal
display output name, and EXTERNALS is a list of external display output names."
  (let ((internal-output nil)
        (external-outputs '()))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      ;; Regex to match connected outputs and check for 'primary'
      (while (re-search-forward "^\\([^ ]+\\) connected\\( primary\\)?" nil t)
        (let ((output (match-string 1))
              (is-primary (match-string 2)))
          (if is-primary
              (progn
                (message "Internal (primary) display detected: %s" output)
                (setq internal-output output))
            (message "External display detected: %s" output)
            (push output external-outputs)))))
    ;; If no output is marked as primary, select the first connected output as internal
    (unless internal-output
      (message "No primary display detected; selecting first connected output as internal display")
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (when (re-search-forward "^\\([^ ]+\\) connected" nil t)
          (setq internal-output (match-string 1))
          (message "Selected internal display: %s" internal-output)
          ;; Remove the selected internal output from external outputs if present
          (setq external-outputs (remove internal-output external-outputs)))))
    (cons internal-output (reverse external-outputs))))

(defun wm/build-xrandr-command (internal-output external-outputs)
  "Build the xrandr command based on INTERNAL-OUTPUT and EXTERNAL-OUTPUTS."
  (let ((xrandr-command (format "xrandr --output %s --auto" internal-output)))
    (dolist (output external-outputs)
      (setq xrandr-command
            (concat xrandr-command
                    (format " --output %s --auto %s %s"
                            output wm/external-monitor-position internal-output))))
    xrandr-command))

(defun wm/update-workspace-monitor-plist (internal-output external-outputs)
  "Update `exwm-randr-workspace-monitor-plist' based on connected outputs.
Assigns workspaces to monitors according to the desired configuration."
  (if external-outputs
      (progn
        (message "External monitors connected: %s" external-outputs)
        ;; Assign workspaces: all to external monitor except workspace 6
        (setq exwm-randr-workspace-monitor-plist
              (cl-loop for i from 0 to 9
                       append (list i (if (= i 6)
                                          internal-output
                                        (car external-outputs))))))
    ;; No external monitors connected
    (progn
      (message "No external monitors detected; all workspaces use internal display")
      (setq exwm-randr-workspace-monitor-plist nil))))  ;; All workspaces use default monitor

(defun wm/update-displays ()
  "Update display configuration based on connected monitors."
  (let* ((outputs (wm/get-connected-outputs))
         (internal-output (car outputs))
         (external-outputs (cdr outputs)))
    (if internal-output
        (progn
          ;; Build and execute xrandr command
          (let ((xrandr-command (wm/build-xrandr-command internal-output external-outputs)))
            (message "Executing xrandr command: %s" xrandr-command)
            (start-process-shell-command "xrandr" nil xrandr-command))
          ;; Update workspace monitor mapping
          (wm/update-workspace-monitor-plist internal-output external-outputs))
      (message "No internal output detected; cannot configure displays"))))

;;; Keybindings

(defun wm/setup-global-keybindings ()
  "Set up global keybindings for EXWM."
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ([?\s-g] . keyboard-quit)
          ;; Switch to other window
          ([?\s-o] . other-window)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Bind "s-d" to launch applications.
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
          )))

;;; Provide Feature

(provide 'plugins/wm/core)
;;; core.el ends here
