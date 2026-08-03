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
;; Code-development plugin: project management, language modes, LSP/DAP,
;; version control, completion, AI assistants and snippets.  General text
;; editing features live in the `editor' plugin.
;;
;;; Code:
(require 'lib/pkg/core)

(defun devel/initialize ()
  "Initilize Noteditor development plugin."

  ;; --- Language IDE support -------------------------------------------------
  ;; Which languages get full IDE features (LSP + tree-sitter highlighting).
  ;; Override in noteditor-user.el, e.g. (setq noteditor-devel-languages
  ;; '(python ruby)).  Because the user file loads first and this is a `defvar',
  ;; a value the user set earlier is never clobbered -- their choice wins.
  (defvar noteditor-devel-languages
    '(c-cpp python ruby java typescript javascript)
    "Languages noteditor auto-enables IDE features (LSP + tree-sitter) for.
Valid symbols: c-cpp python ruby java typescript javascript.")

  (defconst noteditor--language-hooks
    '((c-cpp      . (c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
      (python     . (python-mode-hook python-ts-mode-hook))
      (ruby       . (ruby-mode-hook ruby-ts-mode-hook))
      (java       . (java-mode-hook java-ts-mode-hook))
      (typescript . (typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
      (javascript . (js-mode-hook js-ts-mode-hook js2-mode-hook)))
    "Major-mode hooks that should auto-start LSP, keyed by language symbol.")

  (defconst noteditor--language-ts-langs
    '((c-cpp      . (c cpp))
      (python     . (python))
      (ruby       . (ruby))
      (java       . (java))
      (typescript . (typescript tsx))
      (javascript . (javascript)))
    "Tree-sitter grammar symbols per language, driving `treesit-auto'.")

  (defun noteditor--enable-language-lsp ()
    "Attach `lsp-deferred' to every enabled language's major-mode hooks.
Both classic and `*-ts-mode' hooks are wired so LSP activation does not
depend on whether tree-sitter remapping succeeded."
    (dolist (lang noteditor-devel-languages)
      (dolist (hook (alist-get lang noteditor--language-hooks))
        (add-hook hook #'lsp-deferred))))
  (noteditor--enable-language-lsp)

  (pkg/use projectile
    :init
    (projectile-mode +1)
    :config
    (progn
      ;; We don't want the auto discovery on startup
      ;; (setq projectile-auto-discover nil)
      (setq projectile-indexing-method 'native)
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
  ;; JavaScript indentation
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js-indent-level 2)
              (setq indent-tabs-mode nil)))

  ;; TypeScript indentation
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq typescript-indent-level 4)
              (setq indent-tabs-mode nil)))
  ;; TypeScript/JavaScript LSP activation is handled by
  ;; `noteditor--enable-language-lsp' above.

  ;; Enable lsp-mode for Svelte files
  (pkg/use svelte-mode)
  (add-hook 'svelte-mode-hook #'lsp)

  (add-hook 'svelte-mode-hook
          (lambda ()
            (setq js-indent-level 4)
            (setq css-indent-offset 4)
            (setq indent-tabs-mode nil)))

  ;; Ruby (`ruby-mode'/`ruby-ts-mode' are built in).  Prefer the `ruby-lsp'
  ;; server; the rubocop and typeprof LSP clients are noisy and fail to start
  ;; without dedicated project setup, so keep them disabled.
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-disabled-clients 'rubocop-ls)
    (add-to-list 'lsp-disabled-clients 'typeprof-ls))
  ;; Ruby LSP activation is handled by `noteditor--enable-language-lsp' above.

  (pkg/use ag)

  (pkg/use helm-ag
    :after projectile)

  (pkg/use rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (pkg/use treemacs-projectile
    :after (treemacs projectile))

  (pkg/use magit
    :bind ("C-x g" . magit-status))

  (pkg/use diff-hl
    :init
    (global-diff-hl-mode))

  (pkg/use nix-mode
    :mode "\\.nix\\'")
  ;; Add nix-mode to the lsp-mode hook
  (add-hook 'nix-mode-hook #'lsp)

  ;; LSP and DAP Mode Configuration
  (pkg/use lsp-mode
    :commands (lsp lsp-deferred)
    :init
    ;; `lsp-keymap-prefix' must be set before lsp-mode is loaded.
    (setq lsp-keymap-prefix "C-c l") ;; Use "C-c l" as the prefix for lsp commands
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-enable-indentation nil)
    (setq lsp-enable-on-type-formatting nil)
    ;; Route diagnostics through flycheck (configured below).
    (setq lsp-diagnostics-provider :flycheck)
    :bind (:map lsp-mode-map
                ;; xref's `M-.'/`M-?' already work via lsp's xref backend; make
                ;; the intent explicit and add a mnemonic references binding.
                ("M-." . lsp-find-definition)
                ("M-?" . lsp-find-references))
    :config
    (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("nixd"))
                      :major-modes '(nix-mode)
                      :server-id 'nixd)))

  (pkg/use lsp-ui
    :init
    (progn
      (setq lsp-ui-doc-enable t
            lsp-ui-doc-show-with-cursor t))
    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    ;; Peek-style jump to definition/references (popup with preview).
    (define-key lsp-ui-mode-map [remap xref-find-definitions]
                #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references]
                #'lsp-ui-peek-find-references))

  (pkg/use helm-lsp :commands helm-lsp-workspace-symbol)

  (pkg/use lsp-treemacs :commands lsp-treemacs-errors-list)

  ;; Java Support.  Point lsp-java at the hermetic, Nix-provided jdtls (set via
  ;; NOTEDITOR_JDTLS_HOME in the wrapper) instead of its runtime auto-download.
  ;; LSP activation for java-mode/java-ts-mode is handled by the enable loop.
  (pkg/use lsp-java
    :init
    (when-let ((jdtls-home (getenv "NOTEDITOR_JDTLS_HOME")))
      (setq lsp-java-server-install-dir jdtls-home)))

  ;; DAP Mode for Debugging
  (pkg/use dap-mode
    :after lsp-mode
    :config
    (dap-auto-configure-mode)
    (require 'dap-python)
    (setq dap-python-executable "python3")
    ;; Set default debug template
    (setq dap-python-debugger 'debugpy)
    ;; Enable DAP mode for Python
    (require 'dap-java)   ;; Enable DAP mode for Java
    ;; C/C++ via LLVM's DAP adapter (bundled: pkgs.lldb -> `lldb-dap').  Chosen
    ;; over dap-cpptools, which downloads the VS Code extension at runtime.
    (require 'dap-lldb)
    (setq dap-lldb-debug-program '("lldb-dap")))

  ;; On-the-fly linting.  lsp-mode feeds its diagnostics through flycheck
  ;; (see `lsp-diagnostics-provider' above); flycheck also covers non-LSP
  ;; buffers.  `:defer nil' loads it eagerly so the global mode is live at
  ;; startup (the `pkg/use' macro force-defers unless `:defer' is given).
  (pkg/use flycheck
    :defer nil
    :config
    (global-flycheck-mode))

  ;; Tree-sitter: auto-remap classic major modes to the built-in `*-ts-mode'
  ;; per file type, using grammars baked in by Nix.  `treesit-auto' falls back
  ;; to the classic mode when a grammar is missing or ABI-incompatible.
  ;; `:defer nil' so `global-treesit-auto-mode' is active before files open.
  (pkg/use treesit-auto
    :defer nil
    :init
    (setq treesit-auto-install nil) ;; grammars come from Nix; never fetch
    (setq treesit-auto-langs
          (delete-dups
           (mapcan (lambda (lang)
                     (copy-sequence
                      (alist-get lang noteditor--language-ts-langs)))
                   noteditor-devel-languages)))
    :config
    (global-treesit-auto-mode))

  ;; Optional: Hydra for easier control
  (pkg/use hydra)

  (pkg/use edbi)

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
    :ensure t)

  ;; Enable copilot only when the package is actually present.  copilot.el is
  ;; pulled via a `:straight' recipe that Nix strips, so on a stock build
  ;; `copilot-mode' is void; calling it bare from `prog-mode-hook' would signal
  ;; an error that aborts the whole hook chain -- which would stop `lsp' (and
  ;; every other mode-hook) from ever running.  Guard it so LSP always starts.
  (defun noteditor--maybe-enable-copilot ()
    "Turn on `copilot-mode' iff the copilot package is available."
    (when (fboundp 'copilot-mode)
      (copilot-mode 1)))
  (add-hook 'prog-mode-hook #'noteditor--maybe-enable-copilot)
  (add-hook 'yaml-mode-hook #'noteditor--maybe-enable-copilot)
  (add-hook 'web-mode-hook #'noteditor--maybe-enable-copilot)

  ; AI Assistant
  (use-package aidermacs
    :bind (("C-c a" . aidermacs-transient-menu))
    :custom
    (aidermacs-default-chat-mode 'coder)
    ;;(aidermacs-default-model "openrouter/anthropic/claude-sonnet-4")
    (aidermacs-default-model "openai/gpt-5")

    ;; FREE architect/reasoning model
    ;; (aidermacs-architect-model "openrouter/auto")        ; let OR decide
    ;; (aidermacs-architect-model "deepseek/deepseek-v3-0324:free")
    ;; (aidermacs-architect-model "mistralai/mixtral-8x7b-instruct:free")

    ;; Optional: a tiny “weak” model for commit messages & summaries
    (aidermacs-weak-model "openrouter/meta-llama/llama-3-8b-instruct:free"))

  (use-package shell-maker
    :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

  (use-package copilot-chat
    :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
    :custom
    (copilot-chat-frontend 'shell-maker)
    :config
    (require 'copilot-chat-shell-maker)
    (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list))

  (pkg/use yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

  (pkg/use yasnippet)

(require 'subr-x) ;; for string-trim
(require 'aidermacs)

(defun aidermacs--project-root-or-default ()
  "Return a sensible project root: projectile, vc, or `default-directory`."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (and (fboundp 'vc-root-dir) (vc-root-dir))
      default-directory))

(defun aidermacs-add-files-from-current-buffer (&optional use-region)
  "Add all files listed (one per line) in the current buffer to Aidermacs.
If USE-REGION (prefix arg) is non-nil and a region is active,
use only the region.

Supports absolute and relative paths. Relative paths are resolved
from the project root when possible. Lines starting with # are ignored."
  (interactive "P")
  (let* ((raw (if (and use-region (use-region-p))
                  (buffer-substring-no-properties (region-beginning)
                                                 (region-end))
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
         (lines (split-string raw "\n" t))
         (proj-root (aidermacs--project-root-or-default))
         added skipped)

    (dolist (ln lines)
      (let ((s (string-trim ln)))
        (when (and (not (string-empty-p s))
                   (not (string-prefix-p "#" s)))
          (let* ((path (if (file-name-absolute-p s)
                           (expand-file-name s)
                         (expand-file-name s proj-root)))
                 (path (if (string-suffix-p "/" path)
                           (directory-file-name path)
                         path)))
            (if (file-exists-p path)
                (condition-case err
                    (progn
                      ;; IMPORTANT: Use funcall so the interactive prompt is not triggered
                      (funcall 'aidermacs-add-file path)
                      (push path added))
                  (error (message "Error adding %s: %S" path err)))
              (push path skipped))))))

    ;; Messages
    (when added
      (message "Added %d files to Aidermacs." (length added)))

    (when skipped
      (dolist (p (reverse skipped))
        (message "⚠️  Skipping missing file: %s" p)))

    (list :added (nreverse added)
          :skipped (nreverse skipped))))

(global-set-key (kbd "C-c f") 'aidermacs-add-files-from-current-buffer)


)

(provide 'plugins/devel/core)
;;; core.el ends here
