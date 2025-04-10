{ config, pkgs, extraPkgs, ... }:

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

  home.packages = with pkgs; [
    nix-bash-completions
    tmux
    htop
    wget
    coreutils
    bash-completion
    ripgrep
    tree
    ffmpeg
    zip
    nodejs
    python3
    python3Packages.python-lsp-server
    opam
    ocamlformat
    ocamlPackages.ocaml-lsp
    gdb
    neovim
    wine64
  ] ++ extraPkgs;
}
