{
  config,
  pkgs,
  extraPkgs,
  ...
}:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "s";
  home.homeDirectory = "/home/s";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  fonts.fontconfig.enable = true;

  home.packages =
    with pkgs;
    let
      common = (import ../common.nix { inherit pkgs; });
      pixelsize = 11;
      uiPkgs = common.fonts ++ [
        (common.st { inherit pixelsize; })
        (common.dwm { inherit pixelsize; })
        (common.stSized { pixelsize = 12; })
        (common.stSized { pixelsize = 14; })
        (common.stSized { pixelsize = 16; })
        (common.stSized { pixelsize = 20; })
        (common.stSized { pixelsize = 30; })
        (common.stSized { pixelsize = 40; })
        dmenu
      ];
      devPkgs = [
        binutils
        gcc
        gdb
        gnumake
        openssl
        curlFull
        pkg-config
        python3
        python3Packages.python-lsp-server
        black
        opam
        nodejs
        typescript
        typescript-language-server
        rustc
        cargo
        clippy
        rustPlatform.rustLibSrc
        rust-analyzer
        cargo-watch
        rustfmt
        shellcheck
        nil
        nixfmt
        jujutsu
        ctags
      ];
      toolPkgs = [
        obs-studio
        flowblade
        ledger
        ispell
        vivaldi
        mutt
        zoom-us
        zathura
        nomacs
        vlc
        mplayer
        gimp
        inkscape
        ffmpeg
        fzf
        pavucontrol
        megasync
        file
        discord
        ncdu
        wine
        arandr
        nmap
        unzip
        p7zip
        find-cursor
        gh
        audacity
        imagemagick
        tmate
        dig
        sshfs
        kicad
        freecad
        ripgrep
        fd
        bat
        eza
        dua
        broot
        duf
        remmina
        w3m
        urlscan
        qpdf
        neovim
        vim
        net-tools
        traceroute
        wireshark-qt
      ];
    in
    uiPkgs ++ devPkgs ++ toolPkgs ++ extraPkgs;

  # Allows rust-analyzer to find the rust source
  home.sessionVariables = {
    RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
  };
}
