{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/System/PosixCompat/Unistd.hs" #-}











































{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
This module makes the operations exported by @System.Posix.Unistd@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.Unistd@, on other platforms it emulates the operations as far
as possible.
-}
module System.PosixCompat.Unistd (
    -- * System environment
      SystemID(..)
    , getSystemID
    -- * Sleeping
    , sleep
    , usleep
    , nanosleep
    ) where


import System.Posix.Unistd

