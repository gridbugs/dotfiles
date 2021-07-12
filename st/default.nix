self: super:

with super;

let
  bold-is-not-bright = fetchurl {
    url = "https://st.suckless.org/patches/bold-is-not-bright/st-bold-is-not-bright-20190127-3be4cf1.diff";
    sha256 = "329169acac7ceaf901995d6e0897913089b799d8cd150c7f04c902f4a5b8eab2";
  };
  scrollback = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.2.diff";
    sha256 = "9c5aedce2ff191437bdb78aa70894c3c91a47e1be48465286f42d046677fd166";
  };
  scrollback-mouse = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-0.8.2.diff";
    sha256 = "6103a650f62b5d07672eee9e01e3f4062525083da6ba063e139ca7d9fd58a1ba";
  };
  scrollback-mouse-altscreen = fetchurl {
    url = "https://st.suckless.org/patches/scrollback/st-scrollback-mouse-altscreen-0.8.diff";
    sha256 = "bcfc106089d9eb75aa014d4915ed3e6842f1df54edd8b75597154096333df6fa";
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
