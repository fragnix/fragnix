#!/bin/bash
# compile and serve fragnix frontend
PATH=$PATH:$(pwd)/.cabal-sandbox/bin
cd gui-src/elm && ./optimize.sh && cd ../.. && cabal install && cd fragnix/slices && fragnix-browse
