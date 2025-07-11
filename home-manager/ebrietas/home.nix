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

  home.packages = with pkgs;
    let
      toolPkgs = [
        nix-bash-completions
        bash-completion
        tmux
        htop
        ripgrep
        fzf
        fd
        bat
        eza
        dua
        wget
        sshfs
        tree
        ffmpeg
        yt-dlp
        ledger
        zip
        gh
        imagemagick
        p7zip
        ispell
        mutt
        w3m
        urlscan
        qpdf
        neovim
      ];
      devPkgs = [
        nodejs
        python3
        python3Packages.python-lsp-server
        opam
        ocaml
        git
        mercurial
        darcs
        shellcheck
        nixfmt-classic
        nil
      ];
    in toolPkgs ++ devPkgs ++ extraPkgs;
}
