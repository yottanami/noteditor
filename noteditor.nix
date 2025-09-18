{ config, pkgs, lib, ... }:

with lib;

let
  noteditorVersion = "0.2.4";
  # Instead of reading from the environment, we now use a module option.
  noteditorPath = config.services.xserver.windowManager.noteditor.path;
  wmCfg = config.services.xserver.windowManager.noteditor;
in {
  options.services.xserver.windowManager.noteditor = {
    enable = mkEnableOption (mdDoc "Enable the noteditor window manager (Emacs with EXWM).");
    enableDefaultConfig = mkOption {
      default = true;
      type = types.bool;
      description = mdDoc "Enable the default configuration for the noteditor window manager.";
    };
    version = mkOption {
      default = noteditorVersion;
      type = types.str;
      example = "0.2.3";
      description = mdDoc "A version label for informational or logging purposes.";
    };
    # New option: the absolute path to your noteditor-wm script.
    path = mkOption {
      default = "/home/yottanami/src/personal/noteditor";
      type = types.str;
      description = mdDoc "Absolute path to the directory containing the noteditor-wm script.";
    };
  };

  options.services.dunst = {
    enable = mkEnableOption (mdDoc "Enable the Dunst notification daemon.");
    settings = mkOption {
      type = types.attrs;
      default = {};
      description = mdDoc "Settings for the Dunst notification daemon.";
    };
  };

  config = mkIf wmCfg.enable {
    ######################################
    # 1. Enable EXWM & Register the Session
    ######################################
    services.xserver.windowManager.exwm.enable = true;

    services.xserver.windowManager.session = [
      {
        name = "noteditor";
        bgSupport = true;
        start = ''${noteditorPath}/noteditor-wm'';
      }
    ];

    ######################################
    # 2. Configure Dunst
    ######################################
    services.dunst.enable = true;
    services.dunst.settings = {
      global = {
        browser = "${pkgs.brave}/bin/brave --new-tab";
        follow = "mouse";
        font   = "Droid Sans 16";
        format = "<b>%s</b>\\n%b";
        frame_color = "#555555";
        frame_width = 2;
        # Increase the height of the notification window from 5 to 150.
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
        close   = "mod4+shift+space";
      };
    };

    systemd.user.services.dunst = {
      description = "Dunst Notification Daemon";
      after = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.dunst}/bin/dunst";
        Restart = "always";
      };
      wantedBy = [ "default.target" ];
    };

    ######################################
    # 3. Environment Variables and Packages
    ######################################
    environment.sessionVariables = {
      PATH = [ noteditorPath ];
      NOTEDITOR_VERSION = wmCfg.version;
    };

    environment.systemPackages = with pkgs; [
      emacs
      fd
      silver-searcher
      nodejs
      dunst
      libnotify
      llvmPackages.clangd  # Use clangd from llvmPackages.
      # jdtls is omitted because it is unavailable in current nixpkgs.
      python3            # Updated from python to python3.
      python3Packages.python-lsp-server
      ruby
    ];
  };
}
