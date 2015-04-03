
Fragnix [![Build Status](https://travis-ci.org/phischu/fragnix.svg?branch=master)](https://travis-ci.org/phischu/fragnix)
=======

Fragment-based code distribution!

Installation
------------

Unfourtunately installation is rather difficult, because `fragnix` uses modified versions of `haskell-names` and `haskell-src-exts` that are not (yet) on hackage.

    git clone https://github.com/phischu/fragnix
    cd fragnix
    git submodule init
    git submodule update
    cabal sandbox init
    cabal sandbox add-source haskell-src-exts
    cabal sandbox add-source haskell-names
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build

Example
-------

If you have completed the installation a fragnix executable is in `./dist/build/fragnix`. Example modules are in `./tests/quick`. For example if you invoke:
    
    ./dist/build/fragnix/fragnix ./tests/quick/HelloFragnix/*.hs

You should see the following output:

    [1 of 2] Compiling F8632002673072373674 ( fragnix/temp/compilationunits/F8632002673072373674.hs, fragnix/temp/compilationunits/F8632002673072373674.o )
    [2 of 2] Compiling F3018125790947100434 ( fragnix/temp/compilationunits/F3018125790947100434.hs, fragnix/temp/compilationunits/F3018125790947100434.o )
    Linking main ...

You should be able to execute `main` which prints `"Hello Fragnix!"` to stdout.

