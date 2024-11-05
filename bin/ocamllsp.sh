#!/bin/sh

set -eu

# Script to choose an appropriate instance of ocamllsp. If the project has a
# lockdir then ocamllsp will be installed and run as a dev-tool. Otherwise the
# script will try to launch an opam installation of ocamllsp.

if [ -d dune.lock ]; then

    OCAMLFORMAT_TARGET=_build/_private/default/.dev-tool/ocamlformat/ocamlformat/target

    if [ ! -f $OCAMLFORMAT_TARGET/cookie ]; then
        # Make sure that the ocamlformat dev tool is installed as it's needed by
        # ocamllsp. There's currently no command that just installs ocamlformat so
        # we need to run it and ignore the result.
        dune tools exec ocamlformat -- --help > /dev/null
    fi

    # Add ocamlformat to the environment in which ocamllsp runs so ocamllsp can invoke ocamlformat.
    export PATH=$PWD/$OCAMLFORMAT_TARGET/bin:$PATH

    # Build and run ocamllsp.
    dune tools exec ocamllsp -- $@
else

    # Fallback behaviour for the case where we're not in a dune project with a
    # lockdir. This script includes fallback behaviour so that the same editor
    # configuration can be used to edit ocaml projects that use dune package
    # management as well as ocaml projects that do not.

    script_dir=$(dirname $(realpath $0))

    # Remove the directory containing this script from PATH so it doesn't call itself
    export PATH=$(echo $PATH | tr ':' '\n' | grep -v "^$script_dir$" | paste -sd ':')

    if type opam 2> /dev/null > /dev/null; then

        # If opam is installed then use it to run ocamllsp from the current
        # opam switch. Note that if the user runs `eval $(opam env)` then the
        # bin path from the current opam switch will be in PATH before the
        # directory containing this wrapper script. Therefore the only way that
        # this script can be invoked is if `eval $(opam env)` hasn't been run
        # in the current shell, or ocaml-lsp-server isn't installed in the
        # current opam switch. So in order to run the ocamllsp executable from
        # the current opam switch (assuming it is installed) we have to run it
        # with `opam exec`.
        opam exec ocamllsp -- $@
    else

        # Fall back to running ocamllsp from PATH. This requires ocamllsp to be
        # installed globally which is not recommended because ocamllsp must be
        # compiled with the same version of the compiler as is used to compile
        # the code that it's analyzing. Still there's a chance it could work so
        # try it as a last resort.
        ocamllsp $@
    fi
fi
