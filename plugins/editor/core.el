;;; Noteditor -*- lexical-binding: t; -*-
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
;; General purpose text editing plugin: basic UI, navigation, search and
;; window management.  Code-development features live in the `devel' plugin.
;;
;;; Code:
(require 'lib/pkg/core)

(defun editor/initialize ()
  "Initilize Noteditor editor plugin."

  (ido-mode 1)                        ;; Better open file completion also for projectile
  (display-line-numbers-mode 1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode t)
  (show-paren-mode t)
  (electric-pair-mode 1)
  (delete-selection-mode 1)           ;; Yank the region on type
  (setq inhibit-splash-screen t)      ;; Remove splash screen
  (setq initial-scratch-message nil)  ;; scratch should be scratch
  (setq ido-enable-flex-matching t)   ;; Enables fuzzy matching
  (setq ido-everywhere t)             ;; Use ido for more completion tasks
  ;; Changes the way ido displays the completion list
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  ;; Nix-provided tools are wired in via PATH (see nix/package.nix's
  ;; makeWrapper), not baked-in store paths, so resolve the shell the same
  ;; way: prefer fish if it's on PATH (e.g. via extraRuntimeInputs), fall
  ;; back to the user's own $SHELL, then a plain sh. Works whether or not
  ;; the host is NixOS, and whether or not this is even a Nix-built run.
  ;; $SHELL is treated as unset when empty: some minimal/broken setups
  ;; export SHELL="" rather than leaving it unset, and an empty string is
  ;; non-nil in elisp, so a plain (or ... (getenv "SHELL") ...) would
  ;; silently accept it and skip the bash/sh fallbacks below.
  (setq-default explicit-shell-file-name
                 (or (executable-find "fish")
                     (let ((sh (getenv "SHELL")))
                       (and sh (not (string-empty-p sh)) sh))
                     (executable-find "bash")
                     "/bin/sh"))

  ;; Ensures that Emacs inherits the PATH and other environment variables from your shell.
  (pkg/use exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))

  (pkg/use smart-mode-line
    :straight (smart-mode-line :source melpa)
    :defer nil
    :init
    (progn
      (setq sml/theme 'respectful)
      (setq sml/no-confirm-load-theme t)
      (sml/setup)))

  (pkg/use discover)

  ;; Jump to the things
  (pkg/use avy
    :bind ("s-j" . avy-goto-word-1))

  (pkg/use ace-window
    :bind ("C-<tab>" . ace-window))

  (pkg/use ctrlf
    :defer t
    :init
    (ctrlf-mode +1))

  (pkg/use treemacs)

  (pkg/use which-key
    :config
    (which-key-mode))

  (defun kill-all-buffers ()
  "Kill all buffers. Kill 'Em All!"
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
  ;; Define a keyboard shortcut for the kill-other-buffers function
  (global-set-key (kbd "C-x C-k") 'kill-all-buffers)

)

(provide 'plugins/editor/core)
;;; core.el ends here
