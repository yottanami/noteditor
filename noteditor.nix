{ config, lib, pkgs, ... }:

with lib;

let
  #noteditorPath = lib.environment.getEnv "NOTEDITOR_WM_PATH";
  noteditorPath = builtins.getEnv "NOTEDITOR_WM_PATH";
  cfg = config.services.xserver.windowManager.noteditor;
in

{
  options = {
    services.xserver.windowManager.noteditor = {
      enable = mkEnableOption (lib.mdDoc "noteditor");
      enableDefaultConfig = mkOption {
        default = true;
        type = lib.types.bool;
        description = lib.mdDoc "Enable the default configuration for the noteditor window manager";
      };
    };

    services.dunst = {
      dependsOn = [ pkgs.dunst ];
      enable = mkEnableOption (lib.mdDoc "dunst notification daemon");
      settings = {
        global = {
	  browser = config.programs.librewolf.bin / librewolf -new-tab;
          follow = "mouse";
          font = "Droid Sans 10";
          format = "<b>%s</b>\\n%b";
          frame_color = "#555555";
          frame_width = 2;
          geometry = "500x5-5+30";
          horizontal_padding = 8;
          icon_position = "off";
          line_height = 0;
          markup = "full";
          padding = 8;
          separator_color = "frame";
          separator_height = 2;
          transparency = 10;
          word_wrap = true;
        };

        urgency_low = {
          background = "#1d1f21";
          foreground = "#4da1af";
          frame_color = "#4da1af";
          timeout = 10;
        };

        urgency_normal = {
          background = "#1d1f21";
          foreground = "#70a040";
          frame_color = "#70a040";
          timeout = 15;
        };

        urgency_critical = {
          background = "#1d1f21";
          foreground = "#dd5633";
          frame_color = "#dd5633";
          timeout = 0;
        };

        shortcuts = {
          context = "mod4+grave";
          close = "mod4+shift+space";
        };
      };
    };
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager.session = singleton {
      name = "noteditor";
      bgSupport = true;
      start = '' ${noteditorPath} '';	
    };
    environment.systemPackages = [ pkgs.emacs pkgs.fd pkgs.silver-searcher pkgs.nodejs];
  };
}
