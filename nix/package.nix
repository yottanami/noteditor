# The noteditor package: the elisp tree plus `noteditor` (editor) and
# `noteditor-wm` (window manager) launchers, with every runtime tool wired
# in via the wrapper's PATH.  No absolute paths, no runtime downloads.
{
  lib,
  pkgs,
  stdenvNoCC,
  makeWrapper,
  noteditorEmacs,

  # Runtime tools bundled by default (all free, all in nixpkgs).
  fd,
  nixd,
  nodejs,
  clang-tools,
  python3,
  xrandr,
  flameshot,
  openssh,

  # Language servers / debug adapters for the IDE plugin.
  jdt-language-server, # Java LSP; provides the `jdtls` launcher
  jdk21, # JDK 21, required by jdtls and dap-java
  lldb, # provides the C/C++ DAP adapter (`lldb-dap`)

  # Extra runtime tools the user can append, e.g. a browser, media player or
  # terminal that is unfree or not in nixpkgs (brave, plexamp, alacritty).
  # Kept out of the default closure so the package stays free.
  extraRuntimeInputs ? [ ],

  version ? "0.2.4",
}:

let
  # `silver-searcher' (provides `ag') was renamed to `silver-searcher-ng' in
  # newer nixpkgs; older revisions only have the original name.  Use whichever
  # this nixpkgs provides so the package builds against either.
  silverSearcher = pkgs.silver-searcher-ng or pkgs.silver-searcher;

  # `typescript-language-server' and `typescript' moved to the top level in
  # recent nixpkgs but historically lived under `nodePackages'; accept either so
  # the package builds against old and new revisions alike.
  tsServer = pkgs.typescript-language-server or pkgs.nodePackages.typescript-language-server;
  tsc = pkgs.typescript or pkgs.nodePackages.typescript;

  # python with the LSP server and debug adapter noteditor expects.
  pythonEnv = python3.withPackages (ps: [
    ps.python-lsp-server
    ps.debugpy
  ]);

  runtimeTools = [
    fd
    silverSearcher # provides `ag`
    nixd
    nodejs
    clang-tools # provides `clangd`
    pythonEnv # provides `pylsp` and debugpy
    tsServer # provides `typescript-language-server` (TS/JS LSP)
    tsc # provides `tsc`, the compiler the TS server shells out to
    jdt-language-server # provides `jdtls` (Java LSP)
    jdk21 # JDK for jdtls + dap-java
    lldb # provides `lldb-dap` (C/C++ debug adapter)
    xrandr
    flameshot
    openssh
    pkgs.alacritty # default terminal launcher (s-x) when $TERMINAL isn't set
    pkgs.xdg-utils # provides `xdg-open`, the default browser (s-b) and music-player (s-m) launcher
    pkgs.dunst # notification daemon the WM routes alerts through
    pkgs.xfce4-power-manager # power manager: lid, brightness keys, battery warnings
    pkgs.snixembed # bridges StatusNotifier/AppIndicator icons to XEmbed so exwm-systemtray can show them
    pkgs.networkmanagerapplet # provides `nm-applet` (network tray applet)
    pkgs.blueman # provides `blueman-applet` (bluetooth tray applet)
    pkgs.pasystray # volume/audio tray applet
    pkgs.pavucontrol # GUI mixer that pasystray opens on demand
  ]
  ++ extraRuntimeInputs;

  binPath = lib.makeBinPath runtimeTools;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "noteditor";
  inherit version;

  src = lib.cleanSource ../.;

  nativeBuildInputs = [ makeWrapper ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    # The whole elisp tree goes to one prefix so plugins keep finding their
    # assets via the same relative paths (e.g. theme -> ../../share/images).
    home=$out/share/noteditor
    mkdir -p "$home"
    cp -r core lib plugins noteditor-config.el noteditor-user.el "$home/"
    cp -r share "$home/share"

    # -q skips the user's personal init; site files are kept because the
    # Nix Emacs relies on them to activate its packages and set native-comp
    # and exec paths.
    commonFlags="--name Noteditor -q --no-splash --title Noteditor -l $home/noteditor-config.el"

    makeWrapper ${noteditorEmacs}/bin/emacs $out/bin/noteditor \
      --set NOTEDITOR_HOME "$home" \
      --set NOTEDITOR_NIX true \
      --set NOTEDITOR_WM false \
      --set NOTEDITOR_JDTLS_HOME "${jdt-language-server}/share/java/jdtls" \
      --set JAVA_HOME "${jdk21}" \
      --prefix PATH : ${binPath} \
      --add-flags "$commonFlags"

    makeWrapper ${noteditorEmacs}/bin/emacs $out/bin/noteditor-wm \
      --set NOTEDITOR_HOME "$home" \
      --set NOTEDITOR_NIX true \
      --set NOTEDITOR_WM true \
      --set NOTEDITOR_JDTLS_HOME "${jdt-language-server}/share/java/jdtls" \
      --set JAVA_HOME "${jdk21}" \
      --prefix PATH : ${binPath} \
      --add-flags "$commonFlags"

    # Desktop + session entries, with the @bindir@ placeholder resolved to
    # the store wrappers.
    install -Dm644 share/applications/noteditor.desktop \
      $out/share/applications/noteditor.desktop
    install -Dm644 share/xsessions/noteditor.desktop \
      $out/share/xsessions/noteditor.desktop
    substituteInPlace \
      $out/share/applications/noteditor.desktop \
      $out/share/xsessions/noteditor.desktop \
      --replace-fail '@bindir@' "$out/bin"

    # Icon for the desktop entries.
    install -Dm644 share/images/logo.svg \
      $out/share/icons/hicolor/scalable/apps/noteditor.svg

    runHook postInstall
  '';

  # Lets `services.displayManager.sessionPackages = [ noteditor ]` expose
  # noteditor as a selectable login session, the way every other WM is.
  passthru.providedSessions = [ "noteditor" ];

  meta = {
    description = "Emacs-based IDE and EXWM window manager distribution";
    longDescription = ''
      noteditor is an all-in-one window manager and text editor built on
      Emacs and EXWM.  It runs either as a normal IDE (`noteditor`) or as a
      full X window manager session (`noteditor-wm`).
    '';
    homepage = "https://github.com/yottanami/noteditor";
    license = lib.licenses.gpl3Plus;
    platforms = lib.platforms.linux;
    mainProgram = "noteditor";
    # Not using lib.maintainers.<name> here since this package isn't (and
    # per the project's own nixpkgs-upstream feasibility review, currently
    # isn't planned to be) submitted to nixpkgs, so there's no entry in
    # nixpkgs' maintainer-list.nix to reference.
    maintainers = [
      {
        name = "Behnam Khanbeigi";
        email = "yottanami@gnu.org";
        github = "yottanami";
        githubId = 54559;
      }
    ];
  };
})
