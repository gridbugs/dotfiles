{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "s";
  home.homeDirectory = "/Users/s";

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
    toolPkgs = [
      coreutils
      nix-bash-completions
      bash-completion
      neovim
      tmux
      htop
      ripgrep
      fd
      bat
      eza
      dua
      wget
      sshfs
      fontforge
      tree
      ffmpeg
      youtube-dl
      ledger
      zip
      unrar
      gh
      ncdu
      mosh
      imagemagick
      p7zip
      ispell
    ];
    devPkgs = [
      nodejs
      python3
      python3Packages.python-lsp-server
      dune_3
      opam
      ocamlformat
      ocamlPackages.ocaml-lsp
      git
      mercurial
      darcs
      shellcheck
    ];
  in
  toolPkgs ++ devPkgs;
}
