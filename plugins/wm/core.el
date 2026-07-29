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

;; Which concrete program each app-launcher keybinding starts.  Defaults come
;; from the matching environment variable so the choice of terminal/browser/
;; player lives in the host configuration (e.g. `environment.sessionVariables')
;; rather than being hard-coded here.  Each value is a shell command string.

(defvar wm/terminal (or (getenv "TERMINAL") "alacritty")
  "Shell command the terminal keybinding (s-x) launches.")

(defvar wm/browser (or (getenv "BROWSER") "xdg-open https://")
  "Shell command the browser keybinding (s-b) launches.
Left at the default it opens the freedesktop default browser via `xdg-open'.")

(defvar wm/music-player (or (getenv "MUSIC_PLAYER") "xdg-open")
  "Shell command the music-player keybinding (s-m) launches.")

;;; Initialization

(defun wm/initialize ()
  "Initialize EXWM window manager."
  (interactive)  
  (pkg/use exwm)
  (require 'exwm)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  ;; Default workspace count (formerly handled by `exwm-config-default').
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Hide the menu bar, tool bar (icons) and scroll bar. In WM mode the
  ;; `editor' plugin is not loaded, so this used to be done by
  ;; `exwm-config-default'; restore it here now that we no longer call it.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Make the X window class name the buffer name.
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Set global keybindings (populates `exwm-input-global-keys' before start).
  (wm/setup-global-keybindings)
  ;; Set up screen change hook
  (add-hook 'exwm-randr-screen-change-hook #'wm/update-displays)
  ;; Enable RandR support
  (exwm-randr-mode 1)
  ;; Enable the system tray so tray icons (e.g. network, battery) show.
  (exwm-systemtray-mode 1)
  ;; Start the window manager
  (exwm-wm-mode 1)
  ;; display-time-mode
  (display-time-mode 1)
  (display-battery-mode 1)
  ;; Notification daemon.  Normally started by the systemd `dunst' service;
  ;; `wm/ensure-daemon' only launches a copy if none is already running, so we
  ;; never fight over the notification D-Bus name.
  (wm/ensure-daemon "dunst")
  ;; Power manager: handles lid close, brightness keys and low-battery
  ;; warnings (which it routes through dunst).  Bundled with noteditor and
  ;; requires the UPower service to be enabled on the host.
  (wm/ensure-daemon "xfce4-power-manager")
  ;; StatusNotifier -> XEmbed bridge.  `exwm-systemtray' only speaks the old
  ;; XEmbed tray protocol, but modern applets (blueman-applet, nm-applet,
  ;; pasystray) publish their icons via StatusNotifier/AppIndicator instead and
  ;; exit when no watcher is present.  `snixembed' provides that watcher and
  ;; proxies each icon into an XEmbed one the tray can embed, so it must start
  ;; before the applets below.
  (wm/ensure-daemon "snixembed")
  ;; Tray applets (need the system tray enabled above).  Each requires its
  ;; matching system service on the host: NetworkManager, blueman/bluetooth
  ;; and PipeWire/PulseAudio respectively.
  (wm/ensure-daemon "nm-applet")
  (wm/ensure-daemon "blueman-applet")
  (wm/ensure-daemon "pasystray")
  )

;;; Session daemons

(defun wm/ensure-daemon (name &rest args)
  "Start the program NAME with ARGS unless it is already running.
Uses `pgrep' to avoid launching a duplicate when the same daemon is
already started elsewhere (e.g. by a systemd unit), which would make two
copies fight over a shared D-Bus name.  Does nothing if NAME is not on
the exec PATH."
  (when (and (executable-find name)
             ;; `pgrep -x' exits 0 when a process with this exact name is
             ;; found, non-zero otherwise; only launch when none is found.
             (not (zerop (call-process "pgrep" nil nil nil "-x" name))))
    (apply #'start-process name nil name args)))

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
  (sleep-for 2)
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

(defun wm/launch (command)
  "Start shell COMMAND as a detached process.
Unlike a bare `start-process', this honours multi-word commands such as
\"flameshot gui\".  Warns instead of failing when COMMAND is empty."
  (if (and (stringp command) (not (string-empty-p command)))
      (start-process-shell-command command nil command)
    (message "wm: no application configured for this keybinding")))

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
          ;; Bind "s-l" to "screen lock" (display-manager session locker).
          ;; `dm-tool' is intentionally NOT in this package's own Nix
          ;; closure (nix/package.nix's runtimeTools): it only does
          ;; anything meaningful when LightDM is the active display
          ;; manager, which is a host/session choice, not something
          ;; noteditor's own package should bundle. On a host without
          ;; LightDM this binding is a no-op rather than a broken path.
          ([?\s-l] . (lambda ()
                       (interactive)
                       (wm/launch "dm-tool lock")))
          ;; bind "s-s" to "screenshot".
          ([?\s-s] . (lambda ()
                       (interactive)
                       (wm/launch "flameshot gui")))
          ;; Bind "s-m" to the configured music player.
          ([?\s-m] . (lambda ()
                       (interactive)
                       (wm/launch wm/music-player)))
          ;; Bind "s-b" to the configured web browser.
          ([?\s-b] . (lambda ()
                       (interactive)
                       (wm/launch wm/browser)))
          ;; Bind "s-x" to the configured terminal.
          ([?\s-x] . (lambda ()
                       (interactive)
                       (wm/launch wm/terminal)))
          ;; Bind "s-t" to "tab-bar-mode".
          ([?\s-t] . tab-bar-mode)
          )))

;;; Provide Feature

(provide 'plugins/wm/core)
;;; core.el ends here
