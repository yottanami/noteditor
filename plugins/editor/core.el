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
;;
;;; Code:
(require 'lib/pkg/core)

(defun editor/initialize ()
  ;; "Initilize Noteditor editor plugin."


  (pkg/use projectile
    :init
    (projectile-mode +1)
    :config
    (progn
      ;; We don't want the auto discovery on startup
      (setq projectile-auto-discover nil)
      (setq projectile-enable-caching t)
      (setq projectile-project-search-path "~/src/")
      )
    :bind (:map projectile-mode-map
		("s-p" . projectile-command-map)
		("C-c p" . projectile-command-map)))

  (pkg/use projectile-ripgrep
    :after projectile)

  (pkg/use haml-mode)

  (pkg/use typescript-mode)
  (pkg/use svelte-mode)

  (pkg/use ag)
  (pkg/use helm-ag
    :after projectile)

  (pkg/use smart-mode-line
    :straight (smart-mode-line :source melpa)
    :defer nil
    :init
    (progn
      (setq sml/theme 'respectful)
      (setq sml/no-confirm-load-theme t)
      (sml/setup)))

  (pkg/use discover)


  ;; (pkg/use rainbow-delimiters
  ;;          :hook (prog-mode . rainbow-delimiters-mode))
  ;;  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

  (pkg/use rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode t)
  (show-paren-mode t)
  (electric-pair-mode 1)

  ;; Remove splash screen
  (setq inhibit-splash-screen t)
  ;; scratch should be scratch
  (setq initial-scratch-message nil)
  ;; Don't allow tab as indent

  ;; Yank the region on type
  (delete-selection-mode 1)

  ;; Yank the region on type
  (delete-selection-mode 1)

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

  (pkg/use treemacs-projectile
    :after (treemacs projectile))

  (pkg/use lsp-mode
    :commands lsp
    :init
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-enable-indentation nil)
    (setq lsp-enable-on-type-formatting nil)
    )

  (pkg/use lsp-ui
    :init
    (progn
      (setq lsp-ui-doc-enable t
            lsp-ui-doc-show-with-cursor t)
      )
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    )

  (pkg/use helm-lsp :commands helm-lsp-workspace-symbol)

  ;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  (pkg/use lsp-treemacs :commands lsp-treemacs-errors-list)

  (pkg/use lsp-java)
  (add-hook 'java-mode-hook #'lsp)

  (pkg/use dap-mode :after lsp-mode :config (dap-auto-configure-mode))
  (require 'dap-java)

  (pkg/use hydra)

  (pkg/use edbi)

  ;; optional if you want which-key integration
  (pkg/use which-key
    :config
    (which-key-mode))

  (pkg/use company
    :bind (:map company-active-map
                ("M-n" . company-select-next)
                ("M-p" . company-select-previous)
                ("TAB" . company-complete-common-or-cycle)
		("<tab>" . company-complete-common-or-cycle)
                ("M-d" . company-show-doc-buffer))
    :config
    (progn
      ;; Use Company for completion
      (bind-key [remap completion-at-point] #'company-complete company-mode-map)
      (setq company-show-numbers t)
      (setq company-idle-delay 0)
      (setq company-tooltip-limit 20)
      (setq company-echo-delay 0)
      (setq company-tooltip-align-annotations t)
      (setq company-dabbrev-downcase nil)
      (global-company-mode)))

  (pkg/use company-box
    :after company
    :config
    (add-hook 'company-mode-hook 'company-box-mode))


  (pkg/use copilot
    :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))

    :bind (("M-TAB" . 'copilot-accept-completion-by-word)
	   ("M-<tab>" . 'copilot-accept-completion-by-word)
	   ("s-<tab>" . 'copilot-accept-completion)
	   ("s-TAB" . 'copilot-accept-completion)
	   ("s-n" . 'copilot-next-completion)
	   ("s-p" . 'copilot-previous-completion))
    :ensure t
    )
  (add-hook 'prog-mode-hook 'copilot-mode)
  (add-hook 'yaml-mode-hook 'copilot-mode)
  (add-hook 'web-mode-hook 'copilot-mode)

  (pkg/use yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (pkg/use yasnippet)
  
  )

(provide 'plugins/editor/core)
;;; core.el ends here
