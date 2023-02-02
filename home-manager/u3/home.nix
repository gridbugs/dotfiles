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

  home.packages = with pkgs;
    let
      terminus-font = (import ../../terminus-font/default.nix {} { terminus_font = terminus_font; }).terminus_font;
      st = (import ../../st/common.nix { pkgs = pkgs; pixelsize = 12; }).st;
      mkStSized = { pixelsize }: (import ../../st/sized.nix { pkgs = pkgs; pixelsize = pixelsize;}).st;
      st12 = mkStSized { pixelsize = 12; };
      st16 = mkStSized { pixelsize = 16; };
      st24 = mkStSized { pixelsize = 24; };
      dwm = (import ../../dwm/common.nix { pkgs = pkgs; pixelsize = 12; }).dwm;
      obs = (import ../../nix/obs.nix { pkgs = pkgs; }).obs;
      uiPkgs = [
        terminus-font
        st
        st12
        st16
        st24
        dwm
        dmenu
      ];
      devPkgs = [
        binutils
        gcc
        gnumake
        pkg-config
        python3
        python3Packages.python-lsp-server
        opam
        ocamlformat
        ocamlPackages.ocaml-lsp
        nodejs
        rustc
        cargo
        rustPlatform.rustLibSrc
        rust-analyzer
        cargo-watch
        rustfmt
      ];
      toolPkgs = [
        obs
        neovim
        firefox
        thunderbird
        zoom-us
        zathura
        nomacs
        vlc
        mplayer
        gimp
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
        find-cursor
        gh
      ];
    in
    uiPkgs ++ devPkgs ++ toolPkgs;

  # Allows rust-analyzer to find the rust source
  home.sessionVariables = {
    RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
  };
}
