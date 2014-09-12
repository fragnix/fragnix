
Fragnix
=======

Immutable, fragment-based dependencies!

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
