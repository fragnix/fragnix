{-# LINE 1 "src/System/PosixCompat/Time.hs" #-}











































{-# LANGUAGE CPP #-}

{-|
This module makes the operations exported by @System.Posix.Time@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Time@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Time (
      epochTime
    ) where


import System.Posix.Time


