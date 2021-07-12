self: super:

with super;

let
  bold-is-not-bright = fetchurl {
    url = "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
    sha256 = "329169acac7ceaf901995d6e0897913089b799d8cd150c7f04c902f4a5b8eab2";
  };
  scrollback = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.4.diff";
    sha256 = "0i0fav13sxnsydpllny26139gnzai66222502cplh18iy5fir3j1";
  };
  scrollback-mouse = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-20191024-a2c479c.diff";
    sha256 = "0z961sv4pxa1sxrbhalqzz2ldl7qb26qk9l11zx1hp8rh3cmi51i";
  };
  scrollback-mouse-altscreen = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-altscreen-20200416-5703aa0.diff";
    sha256 = "17avl5bgwlh5ayaqfg01sg9klf828hc0fd36cgzldnl595jyp1yb";
  };
  usercflags = ./usercflags.diff;
  st-with-font = st.overrideAttrs ( oldAttrs: rec {
    preBuild = ''
      buildFlagsArray+=(USERCFLAGS=-DUSERFONT="\"\\\"${"Terminus:pixelsize=16"}\\\"\"")
    '';
  });
in
  {
    st = st-with-font.override {
      patches = [ bold-is-not-bright scrollback scrollback-mouse scrollback-mouse-altscreen usercflags ];
      conf = builtins.readFile ./config.h;
    };
  }
