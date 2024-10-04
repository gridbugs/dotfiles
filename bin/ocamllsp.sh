#!/bin/sh

set -eu

# Script to choose an appropriate instance of ocamllsp. If the project has a
# lockdir then ocamllsp will be installed and run as a dev-tool. Otherwise the
# script will try to launch an opam installation of ocamllsp.

if [ -d dune.lock ]; then

    # Make sure that the ocamlformat dev tool is installed as it's needed by
    # ocamllsp. There's currently no command that just installs ocamlformat so
    # we need to run `dune fmt` and ignore the result.
    echo "Installing ocamlformat..."
    dune fmt --preview > /dev/null 2> /dev/null || true  # the first build will fail because of https://github.com/ocaml/dune/issues/10903
    dune fmt --preview > /dev/null

    # Add ocamlformat to the environment in which ocamllsp runs so ocamllsp can invoke ocamlformat.
    export PATH=$PWD/_build/_private/default/.dev-tool/ocamlformat/ocamlformat/target/bin:$PATH

    # Build and run ocamllsp.
    dune tools exec ocamllsp $@
else

    # Fall back to an opam installation of ocamllsp if there is no lockdir.
    opam exec ocamllsp $@
fi
