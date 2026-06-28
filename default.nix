# Non-flake entry point: `nix-build` produces the noteditor package.
{
  pkgs ? import <nixpkgs> { },
}:

pkgs.callPackage ./nix/package.nix {
  noteditorEmacs = pkgs.callPackage ./nix/emacs.nix { };
  # `silver-searcher` was renamed to `silver-searcher-ng`; tolerate channels
  # from either side of that rename.
  silver-searcher-ng = pkgs.silver-searcher-ng or pkgs.silver-searcher;
}
