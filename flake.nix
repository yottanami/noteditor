{
  description = "noteditor — an Emacs-based IDE and EXWM window manager";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      # Overlay exposing `pkgs.noteditor` (and `pkgs.noteditorEmacs`), so the
      # NixOS module's `mkPackageOption pkgs "noteditor"` resolves.
      overlays.default = final: prev: {
        noteditorEmacs = final.callPackage ./nix/emacs.nix { };
        noteditor = final.callPackage ./nix/package.nix {
          noteditorEmacs = final.noteditorEmacs;
        };
      };

      packages = forAllSystems (
        pkgs:
        let
          noteditorEmacs = pkgs.callPackage ./nix/emacs.nix { };
          noteditor = pkgs.callPackage ./nix/package.nix { inherit noteditorEmacs; };
        in
        {
          default = noteditor;
          inherit noteditor noteditorEmacs;
        }
      );

      apps = forAllSystems (pkgs: {
        default = {
          type = "app";
          program = "${self.packages.${pkgs.system}.noteditor}/bin/noteditor";
        };
        wm = {
          type = "app";
          program = "${self.packages.${pkgs.system}.noteditor}/bin/noteditor-wm";
        };
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            self.packages.${pkgs.system}.noteditorEmacs
            pkgs.fd
            (pkgs.silver-searcher-ng or pkgs.silver-searcher)
            pkgs.nixd
          ];
        };
      });

      nixosModules.default = ./nix/nixos-module.nix;

      formatter = forAllSystems (pkgs: pkgs.nixfmt-rfc-style);
    };
}
