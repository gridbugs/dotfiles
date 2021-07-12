self: super:

with super;

let
  ll2 = "alt/ll2.diff";
  td1 = "alt/td1.diff";
in
  {
    terminus_font = terminus_font.overrideAttrs (old: {
      patches = [ ll2 td1 ];
    });
  }
