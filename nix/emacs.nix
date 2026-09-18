# An Emacs with every package noteditor needs baked in at build time.
#
# All dependencies are resolved here by Nix -- nothing is downloaded at
# runtime.  The result is a single `emacs` binary whose load-path already
# contains each package (native-compiled).
{
  pkgs ? import <nixpkgs> { },
  emacs ? pkgs.emacs,
}:

let
  emacsPkgs = pkgs.emacsPackagesFor emacs;

  # `helm-ag' is not packaged in nixpkgs' emacs set, so build it from source.
  # Pinned to a specific commit for reproducibility.
  helm-ag = emacsPkgs.melpaBuild {
    pname = "helm-ag";
    version = "0.62-unstable-2023-12-31";
    src = pkgs.fetchFromGitHub {
      owner = "syohex";
      repo = "emacs-helm-ag";
      rev = "a7b43d9622ea5dcff3e3e0bb0b7dcc342b272171";
      hash = "sha256-bIuZPMsY0iwkUFOfB6rGno0WvlPtbqqgujwhUb6nTLw=";
    };
    packageRequires = [ emacsPkgs.helm ];
    meta = {
      description = "The silver searcher (ag) integration with helm";
      homepage = "https://github.com/syohex/emacs-helm-ag";
      license = pkgs.lib.licenses.gpl3Plus;
    };
  };
in
emacsPkgs.emacsWithPackages (
  epkgs:
  (with epkgs; [
    # Core / package layer
    use-package

    # Editor plugin
    exec-path-from-shell
    projectile
    projectile-ripgrep
    haml-mode
    typescript-mode
    svelte-mode
    ag
    smart-mode-line
    discover
    rainbow-delimiters
    avy
    ace-window
    ctrlf
    treemacs
    treemacs-projectile
    nix-mode
    lsp-mode
    lsp-ui
    helm-lsp
    lsp-treemacs
    lsp-java
    dap-mode
    hydra
    edbi
    which-key
    company
    company-box
    flycheck # on-the-fly linting; lsp-mode routes diagnostics through it
    treesit-auto # auto-remap classic modes to built-in *-ts-modes, with fallback
    web-mode # classic JSX/TSX/HTML editing
    aidermacs
    copilot
    shell-maker
    copilot-chat
    yaml-mode
    yasnippet

    # Theme plugin
    dracula-theme

    # Window-manager plugin
    exwm

    # Org plugin
    org-roam
  ])
  ++ [
    # Built from source above (missing from nixpkgs).
    helm-ag

    # All tree-sitter grammar `.so` files, baked in hermetically.  The
    # `emacsWithPackages' wrapper special-cases this derivation and appends its
    # grammar dir to `treesit-extra-load-path', so `treesit' finds them with no
    # elisp config and nothing is downloaded at runtime.
    epkgs.treesit-grammars.with-all-grammars
  ]
)
