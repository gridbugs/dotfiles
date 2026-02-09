{
  stdenv,
  fetchurl,
  bdftopcf,
}:

stdenv.mkDerivation {
  name = "kakwafont";

  src = fetchurl {
    url = "https://github.com/kakwa/kakwafont/archive/refs/tags/0.1.1.tar.gz";
    hash = "sha256-c2Q6uZApZB/1UdQbyD2bfYYjot1JJ1sBg++RpXsiQms=";
  };

  buildInputs = [ bdftopcf ];

  installPhase = ''
    make install FONTDIR=$out/share/fonts/misc
  '';
}
