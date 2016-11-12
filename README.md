
Fragnix [![Build Status](https://travis-ci.org/phischu/fragnix.svg?branch=master)](https://travis-ci.org/phischu/fragnix)
=======

Fragnix is an experimental code package manager for Haskell. The central idea is that we should share and reuse code in units of small code fragments instead of in units of packages. The current state of development is technology preview, not even alpha.

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

Fragnix is an experiment to find out if the advantages of fragment-based code distribution can be realized. This is my vision of modern code distribution.

### Lightweight dependencies

I've read the term "inflict a dependency". Reusing code should be something good. You should never think twice about using something existing or rolling your own. Or copy pasting part of a package into your codebase.

### Easy contribution

Some packages are missing helpful functionality. Sometimes we implement a helper function that we are sure someone else must have implemented as well but don't bother to release it. Another common pattern is to release a `packagename-extras` package on hackage. Fragnix allows you to contribute useful functions without going through the maintainer and without releasing (and maintaining) a package. You write the useful function, click submit and it is online for others to enjoy.

### Discoverability

Code forms a giant directed acyclic graph. We can use this graph to rank code search results and recommend related functions. We can find real-world example uses of a given function. "People who have used this function have also used...".

### Immutable code

Code is immutable. What we today call a "change to a function" is actually a different function that happens to have the same name. The old function will never go away. You can use both in your project at the same time. This reduces the tension between stability and evolution and hopefully eliminates one use-case of the C preprocessor.

### First class updates

Your environment is frozen by default. You have to explicitly apply updates. Updates can have metadata (non-breaking, performance, whitespace, ...). Different people can have different policies which updates to apply automatically. There could be a tool for example that (automatically) applies all non-breaking updates to your environment. The hope is that updates have a finer granularity than today. Then you could for example selectively apply a non-breaking bugfix without also getting the breaking changes.

### Platform support

Fragnix reduces your project to a small set of Haskell modules. If you want to build your code on a different platform you can invoke GHC on that platform on the set of generated modules. This should make it easier to build your project on for example Raspberry Pi. If some part of a cabal package is not supported on the target platform the build fails under traditional package-based dependency management. Even if you don't actually use the unsupported part. With fragnix the build will succeed.

### First class environments

Multiple environments for example for beginners, web development, data science and so on can coexist. We see a recent trend to develop custom Preludes. This is the same idea, only fully supported. As long as different environments rely on the same core data types they are compatible.

### Foreign function interface

On some platforms (C, Javascript, JVM) we have to integrate with foreign code. It is an open question where fragnix dependency management will stop.

### Compatibility with multiple Haskell compilers

A Haskell program is a set of modules (according to the Haskell Report 2010). Fragnix produces a set of plain Haskell modules. All the compiler has to do is take this set of modules and produce a program. No special support required. Fragnix should eventually work with Haskell compilers like GHCJS, Haste, GHC, Fay, Frege, Clash, Purescript, UHC, JHC, ETA, HaLVM, CodeWorld and those that are not written yet. Some code fragments will be shared across compilers and some won't.

### Lower binary size and compilation time

Fragnix does dead code elimination by design. Because the dead code elimination is static it helps to avoid compilation of large parts of programs. This should speed up compilation.

### Used code is obvious and explicit

Find out if the code you rely on uses certain unsafe features. Find out if you are affected by a security vulnerability. Find out if any used function is deprecated.

### Cache compilation results forever

Fragnix hashes slices. We can cache the compilation results based on this hash even across machines.

### Display execution results on platform

If we have a code snippet that describes for example a picture or a piece of music we can show that picture or a player for the music right next to the code.

### Integration with source control

We will use text-based formats to make it possible to use existing source control tools to manage your environment.

### Separate metadata from code

The platform will make it possible to annotate slices with comments, upvotes, tags, supported platforms, deprecation, benchmarks, tests, ... This enables crowd-sourced documentation.


Related work
------------

Joe Armstrong wondered [Why do we need modules at all?]](http://lambda-the-ultimate.org/node/5079).

Joachim Breitner [suggested to track dependencies at the level of individual functions, types, etc.](http://nominolo.blogspot.de/2012/08/beyond-package-version-policies.html)

Some people use [`npm`](https://www.npmjs.com/) the way you would use `fragnix`. For example [this guy](https://github.com/jonschlinkert/ansi-green/issues/1). Remember [`left-pad`](https://www.npmjs.com/package/left-pad)?

[This project](https://llogiq.github.io/2016/04/24/nsa.html) dumps the call graph of installed [rust](https://www.rust-lang.org/) packages.

[Interlisp](http://www.ics.uci.edu/~andre/ics228s2006/teitelmanmasinter.pdf) had a tool called masterscope to analyse the static call graph of your program.

