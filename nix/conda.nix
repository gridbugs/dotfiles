# Sets up miniconda.
# Copied from http://www.jaakkoluttinen.fi/blog/conda-on-nixos/

{ pkgs ? import <nixpkgs> {} }:

let

  # Conda installs it's packages and environments under this directory
  installationPath = "~/.conda";

  # Downloaded Miniconda installer
  minicondaScript = pkgs.stdenv.mkDerivation rec {
    name = "miniconda-${version}";
    version = "4.3.11";
    src = pkgs.fetchurl {
      url = "https://repo.continuum.io/miniconda/Miniconda3-${version}-Linux-x86_64.sh";
      sha256 = "1f2g8x1nh8xwcdh09xcra8vd15xqb5crjmpvmc2xza3ggg771zmr";
    };
    # Nothing to unpack.
    unpackPhase = "true";
    # Rename the file so it's easier to use. The file needs to have .sh ending
    # because the installation script does some checks based on that assumption.
    # However, don't add it under $out/bin/ becase we don't really want to use
    # it within our environment. It is called by "conda-install" defined below.
    installPhase = ''
      mkdir -p $out
      cp $src $out/miniconda.sh
    '';
    # Add executable mode here after the fixup phase so that no patching will be
    # done by nix because we want to use this miniconda installer in the FHS
    # user env.
    fixupPhase = ''
      chmod +x $out/miniconda.sh
    '';
  };

  # Wrap miniconda installer so that it is non-interactive and installs into the
  # path specified by installationPath
  conda = pkgs.runCommand "conda-install"
    { buildInputs = [ pkgs.makeWrapper minicondaScript ]; }
    ''
      mkdir -p $out/bin
      makeWrapper                            \
        ${minicondaScript}/miniconda.sh      \
        $out/bin/conda-install               \
        --add-flags "-p ${installationPath}" \
        --add-flags "-b"
    '';

in
(
  pkgs.buildFHSUserEnv {
    name = "conda";
    targetPkgs = pkgs: (
      with pkgs; [

        conda

        # Add here libraries that Conda packages require but aren't provided by
        # Conda because it assumes that the system has them.
        #
        # For instance, for IPython, these can be found using:
        # `LD_DEBUG=libs ipython --pylab`
        xorg.libSM
        xorg.libICE
        xorg.libXrender
        libselinux

        # Just in case one installs a package with pip instead of conda and pip
        # needs to compile some C sources
        gcc
      ]
    );
    profile = ''
      # Add conda to PATH
      export PATH=${installationPath}/bin:$PATH
      # Paths for gcc if compiling some C sources with pip
      export NIX_CFLAGS_COMPILE="-I${installationPath}/include"
      export NIX_CFLAGS_LINK="-L${installationPath}lib"
      # Some other required environment variables
      export FONTCONFIG_FILE=/etc/fonts/fonts.conf
      export QTCOMPOSE=${pkgs.xorg.libX11}/share/X11/locale
    '';
  }
).env
