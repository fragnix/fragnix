# Builtin environment

In order to obtain a working fragnix environment we have to provide a builtin environment for base, ghc-prim and integer-gmp.
Additionally, we have to provide missing c headers and files.

The command `fragnix create-env` uses the files in this directory to generate an initial fragnix folder.
