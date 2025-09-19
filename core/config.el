;;; core/config.el --- Noteditor core configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized configuration variables and user config loader.

;;; Code:

(defgroup noteditor nil
  "Customization group for Noteditor."
  :group 'applications)

(defcustom noteditor-mode 'editor
  "Select Noteditor mode.
Valid values are:
- 'editor: Run as a text editor
- 'wm: Run as a window manager (EXWM)"
  :type '(choice (const :tag "Editor" editor)
                 (const :tag "Window Manager" wm))
  :group 'noteditor)

(defcustom noteditor-config-file (expand-file-name "~/.noteditor/config.el")
  "Path to the user-level Noteditor configuration file."
  :type 'file
  :group 'noteditor)

(defcustom noteditor-terminal "/usr/bin/alacritty"
  "Path to the terminal executable to be launched via keybinding."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'noteditor)

(defcustom noteditor-browser "/usr/bin/brave-browser"
  "Path to the browser executable to be launched via keybinding."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'noteditor)

(defcustom noteditor-screenshot "/usr/bin/gnome-screenshot"
  "Path to the screenshot tool executable."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'noteditor)

(defcustom noteditor-screen-lock "/usr/bin/i3lock"
  "Path to the screen lock executable."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'noteditor)

(defcustom noteditor-media-player nil
  "Path to the media player executable."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'noteditor)

(defcustom noteditor-aidermacs-default-model "openrouter/openai/gpt-5"
  "Default model identifier used by aidermacs."
  :type 'string
  :group 'noteditor)

(defcustom noteditor-project-search-path '("~/src/")
  "List of project roots for Projectile."
  :type '(repeat directory)
  :group 'noteditor)

(defcustom noteditor-supported-languages
  '(nix typescript svelte java python yaml)
  "List of programming languages/features to enable."
  :type '(repeat symbol)
  :group 'noteditor)

(defun noteditor/load-user-config ()
  "Load user-level Noteditor configuration if present."
  (when (and noteditor-config-file
             (file-readable-p noteditor-config-file))
    (load-file noteditor-config-file)))

(noteditor/load-user-config)

(provide 'core/config)
;;; core/config.el ends here
