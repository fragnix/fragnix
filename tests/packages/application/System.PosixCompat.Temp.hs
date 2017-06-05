{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/System/PosixCompat/Temp.hs" #-}

















































{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
This module makes the operations exported by @System.Posix.Temp@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Temp@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Temp (
      mkstemp
    ) where

-- Re-export unix package

import System.Posix.Temp

