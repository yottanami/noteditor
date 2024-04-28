{ config, lib, pkgs, ... }:


with lib;

let
  cfg = config.services.xserver.windowManager.noteditor;
in

{
  options = {
    services.xserver.windowManager.noteditor = {
      enable = mkEnableOption (lib.mdDoc "noteditor");
      enableDefaultConfig = mkOption {
        default = true;
        type = lib.types.bool;
        description = lib.mdDoc "E";
      };
    };
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.session = singleton {
      name = "noteditor";
      bgSupport = true;
      start = ''
        /home/yottanami/src/personal/noteditor/noteditor-wm
      '';
    };
    environment.systemPackages = [ pkgs.emacs pkgs.fd pkgs.silver-searcher pkgs.nodejs];
  };
}
