
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

Vision
------

A few random thoughts.

### Integration with source control

We will use text-based formats to make it possible to use existing source control tools to manage your environment.

### Separate metadata from code

We will make it possible to annotate slices with comments, upvotes, tags, ...

### Lightweight dependencies

I've read the term "inflict a dependency". Reusing code should be something good.

### Platform support

If you want to use code on a different platform, port only the part you really need.

### First class environments

Multiple environments for example for beginners, web development, data science and so on can coexist.

### Discoverability

Use code and and metadata for search, example generation and recommendations.

### First class updates

Code is immutable. You have to explicitly apply updates. Updates can have metadata (non-breaking, performance, whitespace, ...).

### Foreign function interface

On some platforms (C, Javascript) we have to integrate with foreign code.

### Easy contribution

Contribute useful functions without going through the maintainer.

