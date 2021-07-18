self: super:

with super;

import ./common.nix {
  pkgs = super.pkgs;
  fetchurl = super.fetchurl;
  st = super.st;
  pixelsize = 16;
}
