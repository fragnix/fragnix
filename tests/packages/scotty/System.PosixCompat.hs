{-# LINE 1 "src/System/PosixCompat.hs" #-}
# 1 "src/System/PosixCompat.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1






















































































# 8 "<command-line>" 2
# 1 "src/System/PosixCompat.hs"
{-# LANGUAGE CPP #-}

{-|
The @unix-compat@ package provides portable implementations of parts of the
@unix@ package. On POSIX system it re-exports operations from the @unix@
package, on other platforms it emulates the operations as far as possible.
-}
module System.PosixCompat (
      module System.PosixCompat.Files
    , module System.PosixCompat.Temp
    , module System.PosixCompat.Time
    , module System.PosixCompat.Types
    , module System.PosixCompat.Unistd
    , module System.PosixCompat.User
    , usingPortableImpl
    ) where

import System.PosixCompat.Files
import System.PosixCompat.Temp
import System.PosixCompat.Time
import System.PosixCompat.Types
import System.PosixCompat.Unistd
import System.PosixCompat.User

-- | 'True' if unix-compat is using its portable implementation,
--   or 'False' if the unix package is simply being re-exported.
usingPortableImpl :: Bool



usingPortableImpl = False


