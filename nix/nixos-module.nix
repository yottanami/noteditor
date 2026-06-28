# NixOS module for noteditor.
#
# Registers noteditor as a selectable login window-manager session (the
# modern, display-manager-agnostic way) and optionally configures the Dunst
# notification daemon that the WM expects.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.noteditor;
in
{
  options.programs.noteditor = {
    enable = lib.mkEnableOption "noteditor, an Emacs/EXWM window manager and IDE";

    package = lib.mkPackageOption pkgs "noteditor" { };

    enableDunst = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Configure and run the Dunst notification daemon for noteditor.";
    };

    dunstBrowser = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.xdg-utils}/bin/xdg-open";
      defaultText = lib.literalExpression ''"''${pkgs.xdg-utils}/bin/xdg-open"'';
      example = lib.literalExpression ''"''${pkgs.brave}/bin/brave --new-tab"'';
      description = "Command Dunst uses to open links in notifications.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # Makes "noteditor" appear in the session list of any display manager
    # (gdm, sddm, lightdm, greetd) via the package's providedSessions.
    services.displayManager.sessionPackages = [ cfg.package ];

    services.dunst = lib.mkIf cfg.enableDunst {
      enable = true;
      settings = {
        global = {
          browser = cfg.dunstBrowser;
          follow = "mouse";
          font = "Droid Sans 16";
          format = "<b>%s</b>\\n%b";
          frame_color = "#555555";
          frame_width = 2;
          geometry = "500x150-5+30";
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
}
