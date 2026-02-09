let
  pkgs = import <nixpkgs> {
    overlays = [ (import ./default.nix) ];
  };
in
pkgs.uw-ttyp0
