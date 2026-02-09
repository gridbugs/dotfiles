{
  stdenv,
  perl,
  bdftopcf,
  fonttosfnt,
  mkfontscale,
}:

stdenv.mkDerivation {
  name = "uw-ttyp0";

  # from https://people.mpi-inf.mpg.de/~uwe/misc/uw-ttyp0
  src = ./uw-ttyp0-2.1.tar.gz;

  buildInputs = [
    perl
    bdftopcf
    fonttosfnt
    mkfontscale
  ];

  patches = [ ./variants.patch ];

  configurePhase = ''
    ./configure --installdirs=$out/share/fonts/misc
  '';
}
