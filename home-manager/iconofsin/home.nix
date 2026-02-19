{ config, pkgs, ... }:

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
      pixelsize = 16;
      uiPkgs = common.fonts ++ [
        (common.st { inherit pixelsize; })
        (common.dwm { inherit pixelsize; })
        (common.stSized { pixelsize = 16; })
        (common.stSized { pixelsize = 20; })
        (common.stSized { pixelsize = 30; })
        dmenu
      ];
      devPkgs = [
        binutils
        gcc
        gnumake
        gmp
        openssl
        opam
        rustc
        cargo
      ];
      toolPkgs = [
        rsync
        mercurial
        darcs
        neovim
        vivaldi
        nomacs
        vlc
        mplayer
        gimp
        ffmpeg
        fzf
        pavucontrol
        file
        discord
        ncdu
        wine
        arandr
        nmap
        unzip
        pcsx2
        p7zip
        find-cursor
        nil
        nixfmt-classic
      ];
    in
    uiPkgs ++ devPkgs ++ toolPkgs;
}
