
Fragnix
=======

Immutable, fragment-based dependencies!

Manifest
--------

Dependency resolution is NP-complete and therefore must be avoided.

Dependencies have to be immutable.

Dependencies have to be between the smallest possible units.

All updates have to be explicit.

Explicit does not mean manual.

Advantages
----------

No “works for me”.

No “download the internet”.

No “compile the internet”.

No “bit rot”.

Easy distribution.

Easy contribution.

Easy collaboration.

Easy reuse.

Installation
------------

After having installed the latest [Haskell-Platform](https://www.haskell.org/platform/):

    git clone github.com/phischu/fragnix
    cabal sandbox init
    cabal install

Example
-------

    .cabal-sandbox/bin/fragnix examples/tests/HelloFragnix.hs

You should see the following output:

    Resolving tests/examples/HelloFragnix.hs ... 2 slices!
    Inserting ... 2 new!
    [1 of 2] Compiling F7933008894552075151 ( fragnix/modules/F7933008894552075151.hs, fragnix/modules/F7933008894552075151.o )
    [2 of 2] Compiling F5431476755877558931 ( fragnix/modules/F5431476755877558931.hs, fragnix/modules/F5431476755877558931.o )
    Linking main ...

`tests/examples/HelloFragnix.hs` looks like this:

    module Main where
    
    putHello :: String -> IO ()
    putHello x = putStrLn ("Hello " ++ x)
    
    main :: IO ()
    main = putHello "Fragnix!"

`fragnix/slices/5431476755877558931` looks like this:

    {
      "sliceID": 5431476755877558931,
      "fragment": [
        " \nmain :: IO ()",
        "main = putHello \"Fragnix!\""
      ],
      "usages": [
        {
          "reference": {
            "originalModule": "GHC.Types"
          },
          "usedName": {
            "typeIdentifier": "IO"
          },
          "qualification": null
        },
        {
          "reference": {
            "otherSlice": 7933008894552075151
          },
          "usedName": {
            "valueIdentifier": "putHello"
          },
          "qualification": null
        }
      ]
    }

`fragnix/modules/F5431476755877558931.hs` looks like this:

    {-# LANGUAGE NoImplicitPrelude #-}
    module F5431476755877558931 where
    import GHC.Types (IO)
    import F7933008894552075151 (putHello)
     
    main :: IO ()
    main = putHello "Fragnix!"
