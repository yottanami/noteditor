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
  "Initilize Noteditor theme plugin.
Use a different theme in window-manager mode (`NOTEDITOR_WM' is \"true\")
than in normal editor mode."
  (if (string= (getenv "NOTEDITOR_WM") "true")
      (theme/load-wm-theme)
    (pkg/use dracula-theme)
    (load-theme 'dracula t)
    ;; Work around an upstream dracula-theme bug: its spec for
    ;; `lsp-ui-sideline-current-symbol' writes `:box (:color dracula-fg)' with the
    ;; unquoted symbol instead of the color string, which Emacs rejects with
    ;; "Invalid face box" as soon as lsp-ui loads.  Re-set the face with the
    ;; intended color so lsp/lsp-ui can start.
    (custom-theme-set-faces
     'dracula
     '(lsp-ui-sideline-current-symbol
       ((t (:foreground "#f8f8f2" :weight ultra-bold
            :box (:line-width -1 :color "#f8f8f2") :height 0.99))))))
  (set-face-attribute 'default nil :height 120)
  (theme/welcome-message)
  )

(defun theme/load-wm-theme ()
  "Load the window-manager theme: orange background, black footer.
Uses the built-in `leuven' light theme as a base, then overrides the
default background to a pastel orange and the mode-line (the bottom
footer menu) to black."
  (load-theme 'leuven t)
  (let ((background "#f97316")    ; window background (modern orange)
        (footer-bg  "#000000")   ; black
        (footer-fg  "#e8eaed")   ; light text on the footer
        (footer-inactive "#1c1c1c"))
    (set-face-attribute 'default nil :background background)
    (set-face-attribute 'fringe nil :background background)
    (set-face-attribute 'mode-line nil
                        :background footer-bg :foreground footer-fg
                        ;; Same color as the background, so the box reads as
                        ;; vertical padding (taller bar) rather than a border.
                        :box `(:line-width 6 :color ,footer-bg))
    (set-face-attribute 'mode-line-inactive nil
                        :background footer-inactive :foreground "#9aa0aa"
                        :box `(:line-width 6 :color ,footer-inactive))
    ;; Make the buffer name readable on the dark footer (leuven styles it
    ;; for a light background by default).
    (set-face-attribute 'mode-line-buffer-id nil
                        :foreground "#ffd479" :background 'unspecified)))

(defconst theme/plugin-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of the theme plugin, used to locate bundled assets.")

(defvar theme/welcome-image
  (let ((dir (expand-file-name "../../share/images/" theme/plugin-directory)))
    ;; Prefer a user-supplied raster logo if present, else the bundled SVG.
    (let ((png (expand-file-name "logo.png" dir)))
      (if (file-exists-p png) png (expand-file-name "logo.svg" dir))))
  "Path to the image shown, centered, on the Noteditor splash buffer.
A `logo.png' next to the bundled `logo.svg' takes precedence if you add one.")

(defconst theme/welcome-buffer-name "*noteditor*"
  "Name of the Noteditor splash buffer.")

(defconst theme/welcome-url "https://github.com/yottanami/noteditor"
  "URL shown beneath the splash image.")

(defun theme/render-welcome (buffer)
  "Render the splash image centered in BUFFER's window.
Centers the image both horizontally and vertically based on the current
window size, so it stays centered when the window or frame is resized.
Falls back to a short text banner on a non-graphical display or when
`theme/welcome-image' is missing."
  (let ((win (get-buffer-window buffer)))
    (when (window-live-p win)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (text-width (window-text-width win)))
          (erase-buffer)
          (if (and (display-graphic-p)
                   (file-exists-p theme/welcome-image)
                   (image-type-available-p
                    (image-type-from-file-name theme/welcome-image)))
              (let* ((img (create-image theme/welcome-image))
                     ;; Size in character units (cols . lines), as floats.
                     (size (image-size img nil (window-frame win)))
                     ;; +2 accounts for the blank line and URL line below.
                     (top  (max 0 (floor (/ (- (window-text-height win)
                                               (cdr size) 2) 2))))
                     (left (max 0 (floor (/ (- text-width (car size)) 2))))
                     (url-left (max 0 (/ (- text-width
                                            (length theme/welcome-url)) 2))))
                (insert (make-string top ?\n) (make-string left ?\s))
                (insert-image img)
                (insert "\n\n" (make-string url-left ?\s) theme/welcome-url))
            ;; Non-graphical / missing-image fallback.
            (insert "\n\nNoteditor\n\n" theme/welcome-url))
          (goto-char (point-min)))))))

(defun theme/welcome-resize (&optional _frame)
  "Re-center the splash image when its window changes size."
  (let ((buffer (get-buffer theme/welcome-buffer-name)))
    (when (and buffer (get-buffer-window buffer))
      (theme/render-welcome buffer))))

(defun theme/show-welcome ()
  "Create and display the Noteditor splash buffer."
  (let ((buffer (get-buffer-create theme/welcome-buffer-name)))
    (with-current-buffer buffer
      (setq cursor-type nil)
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (theme/render-welcome buffer)
    ;; Keep the image centered across resizes (e.g. monitor hotplug in WM mode).
    (add-hook 'window-size-change-functions #'theme/welcome-resize)))

(defun theme/welcome-message ()
  "Arrange to show the splash buffer once Emacs has finished starting."
  (add-hook 'emacs-startup-hook #'theme/show-welcome))

(provide 'plugins/theme/core)
;;; core.el ends here
