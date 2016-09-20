
Fragnix [![Build Status](https://travis-ci.org/phischu/fragnix.svg?branch=master)](https://travis-ci.org/phischu/fragnix)
=======

Fragnix is an experimental code package manager for Haskell. The central idea is that we should share and reuse code in units of small code fragments instead of in units of packages.

Installation
------------

Follow the following steps to get a development version of fragnix. There is no released version yet.

    git clone https://github.com/phischu/fragnix
    cd fragnix
    cabal sandbox init
    cabal install
    cabal configure
    cabal build

This should place a `fragnix` executable in `dist/build/fragnix/fragnix`.

Example
-------

If you have completed the installation a fragnix executable is in `./dist/build/fragnix`. Example modules are in `./tests/quick`. For example if you invoke:
    
    ./dist/build/fragnix/fragnix ./tests/quick/HelloFragnix/*.hs

You should see the following output:

    [1 of 2] Compiling F8632002673072373674 ( fragnix/temp/compilationunits/F8632002673072373674.hs, fragnix/temp/compilationunits/F8632002673072373674.o )
    [2 of 2] Compiling F3018125790947100434 ( fragnix/temp/compilationunits/F3018125790947100434.hs, fragnix/temp/compilationunits/F3018125790947100434.o )
    Linking main ...

You should be able to execute `main` which prints `"Hello Fragnix!"` to stdout.



