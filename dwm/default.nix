self: super:

with super;

let
  noborder = fetchurl {
    url = "https://dwm.suckless.org/patches/noborder/dwm-noborder-6.2.diff";
    sha256 = "9bbf5f963e5a2d23ae4b8731f0c179a8615de5715a2dbf683fbe02115e24efe0";
  };
  pertag = fetchurl {
    url = "https://dwm.suckless.org/patches/pertag/dwm-pertag-6.2.diff";
    sha256 = "055da0f12dbfde9e50df54e1f2d87966466404a36c056efb94bb21ab03b94b10";
  };
  bottomstack = ./bottomstack-custom.diff;
  usercflags = ./usercflags.diff;
  dwm-with-font = dwm.overrideAttrs ( oldAttrs: rec {
    buildPhase = ''
      make USERCFLAGS=-DUSERFONT="\"\\\"${"Terminus:pixelsize=16"}\\\"\""
    '';
  });
in
  {
    dwm = dwm-with-font.override {
      patches = [ noborder bottomstack pertag usercflags ];
      conf = builtins.readFile ./config.h;
    };
  }
