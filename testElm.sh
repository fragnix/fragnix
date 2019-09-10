#!/bin/bash
# compile and serve fragnix frontend
PATH=$PATH:$(pwd)/.cabal-sandbox/bin
cd gui-src/elm && ./optimize.sh && cd ../.. && cabal install && cd tests/packages/application/fragnix/slices && fragnix-browse
