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

  home.packages = with pkgs;
    let
      terminus-font = (import ../../terminus-font/default.nix { } {
        terminus_font = terminus_font;
      }).terminus_font;
      uw-ttyp0 = (import ../../uw-ttyp0/default.nix { } pkgs).uw-ttyp0;
      st = (import ../../st/common.nix {
        pkgs = pkgs;
        pixelsize = 16;
      }).st;
      mkStSized = { pixelsize }:
        (import ../../st/sized.nix {
          pkgs = pkgs;
          pixelsize = pixelsize;
        }).st;
      st12 = mkStSized { pixelsize = 12; };
      st16 = mkStSized { pixelsize = 16; };
      st24 = mkStSized { pixelsize = 24; };
      dwm = (import ../../dwm/common.nix {
        pkgs = pkgs;
        pixelsize = 16;
      }).dwm;
      obs = (import ../../nix/obs.nix { pkgs = pkgs; }).obs;
      uiPkgs = [ terminus-font uw-ttyp0 st st12 st16 st24 dwm dmenu ];
      devPkgs = [ binutils gcc gnumake gmp openssl opam rustc cargo ];
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
    in uiPkgs ++ devPkgs ++ toolPkgs;
}
