{ pkgs }:
{
  fonts =
    let
      terminus-font =
        (import ../terminus-font/default.nix { } {
          terminus_font = pkgs.terminus_font;
        }).terminus_font;
      uw-ttyp0 = (import ../uw-ttyp0/default.nix { } pkgs).uw-ttyp0;
    in
    [
      terminus-font
      uw-ttyp0
    ];

  st =
    { pixelsize }:
    (import ../st/common.nix {
      inherit pixelsize;
      pkgs = pkgs;
    }).st;

  stSized =
    { pixelsize }:
    (import ../st/sized.nix {
      pkgs = pkgs;
      pixelsize = pixelsize;
    }).st;

  dwm =
    { pixelsize }:
    (import ../dwm/common.nix {
      inherit pixelsize;
      pkgs = pkgs;
    }).dwm;

  devPkgs = with pkgs; [
    # General dev packages
    binutils
    gcc
    gdb
    gnumake
    openssl
    curlFull
    pkg-config

    # C
    clang-tools
    ctags

    # Python
    python3
    python3Packages.python-lsp-server
    black

    # Rust
    rustc
    cargo
    clippy
    rustPlatform.rustLibSrc
    rust-analyzer
    cargo-watch
    rustfmt

    # Nix
    nil
    nixfmt

    # Javascript
    nodejs
    typescript
    typescript-language-server

    # OCaml
    opam

    # Bash
    shellcheck
  ];

  toolPkgs = with pkgs; [
    ledger
    ispell
    mutt
    urlscan
    fzf
    file
    ncdu
    nmap
    unzip
    p7zip
    find-cursor
    ripgrep
    fd
    bat
    eza
    dua
    duf
    w3m
    neovim
    vim
    net-tools
    traceroute
    dig
    ffmpeg
  ];

  toolPkgsGraphical = with pkgs; [
    vivaldi
    zathura
    nomacs
    vlc
    mplayer
    pavucontrol
    megasync
    arandr
    find-cursor
    imagemagick
  ];
}
