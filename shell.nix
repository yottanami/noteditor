# Non-flake dev shell: `nix-shell` gives noteditor's Emacs plus the common
# command-line tools it shells out to.
{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  packages = [
    (pkgs.callPackage ./nix/emacs.nix { })
    pkgs.fd
    (pkgs.silver-searcher-ng or pkgs.silver-searcher)
    pkgs.nixd
  ];
}
