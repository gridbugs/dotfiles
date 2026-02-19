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
}
