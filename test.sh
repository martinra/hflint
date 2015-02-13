#! /bin/bash

cabal configure --extra-lib-dirs=$flint/lib --extra-lib-dirs=$gf2x/lib --extra-lib-dirs=$mpfr/lib --extra-lib-dirs=$ntl/lib --extra-lib-dirs=$gmp/lib --disable-library-profiling
cabal test
