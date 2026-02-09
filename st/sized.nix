{
  pkgs ? import <nixpkgs> { },
  pixelsize ? 16,
}:

{
  st =
    (import ./common.nix {
      pkgs = pkgs;
      pixelsize = pixelsize;
    }).st.overrideAttrs
      (old: {
        pname = "st${toString pixelsize}";

        buildInputs = pkgs.st.buildInputs ++ [ pkgs.terminus_font ];

        # set a preprocessor constant that controls font size
        preBuild = ''
          buildFlagsArray+=(USERCFLAGS=-DUSERFONT="\"\\\"${"Terminus:pixelsize=${toString pixelsize}"}\\\"\"")
        '';

        # rename the binary to include the font size
        postInstall = ''
          mv $out/bin/st{,${toString pixelsize}}
        '';

        # hack to prevent multiple different sized st's conflicting on terminfo entries
        meta.priority = pixelsize;
      });
}
