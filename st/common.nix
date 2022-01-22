{ pkgs ? import <nixpkgs> { }
, fetchurl ? pkgs.fetchurl
, st ? pkgs.st
, pixelsize ? 16
}:

let
  bold-is-not-bright = fetchurl {
    url = "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
    sha256 = "329169acac7ceaf901995d6e0897913089b799d8cd150c7f04c902f4a5b8eab2";
  };
  scrollback = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-20210507-4536f46.diff";
    sha256 = "072icbmj7my4c134d5apqw7v9q88vcrp6v6gdzf3668dzpkz9n0r";
  };
  scrollback-mouse = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20191024-a2c479c.diff";
    sha256 = "0z961sv4pxa1sxrbhalqzz2ldl7qb26qk9l11zx1hp8rh3cmi51i";
  };
  scrollback-mouse-altscreen = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-altscreen-20200416-5703aa0.diff";
    sha256 = "17avl5bgwlh5ayaqfg01sg9klf828hc0fd36cgzldnl595jyp1yb";
  };
  replace-space = ./replace-space.diff;
in
  {
    st = (st.override {
      patches = [
        bold-is-not-bright
        scrollback
        scrollback-mouse
        scrollback-mouse-altscreen
        replace-space
        ./usercflags.diff
      ];
      conf = builtins.readFile ./config.h;
    }).overrideAttrs( old: {

      buildInputs = st.buildInputs ++ [ pkgs.terminus_font ];

      # set a preprocessor constant that controls font size
      preBuild = ''
        buildFlagsArray+=(USERCFLAGS=-DUSERFONT="\"\\\"${"Terminus:pixelsize=${toString pixelsize}"}\\\"\"")
      '';
    });
  }
