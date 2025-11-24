self: super:

with super;

import ./common.nix {
  pkgs = super.pkgs;
  fetchurl = super.fetchurl;
  dwm = super.dwm;
  pixelsize = 16;
}
