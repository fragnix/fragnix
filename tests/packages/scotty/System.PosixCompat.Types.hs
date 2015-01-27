{-# LINE 1 "src/System/PosixCompat/Types.hs" #-}











































{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
This module re-exports the types from @System.Posix.Types@ on all platforms.

On Windows 'UserID', 'GroupID' and 'LinkCount' are missing, so they are
redefined by this module.
-}
module System.PosixCompat.Types (
     module System.Posix.Types
    ) where

import System.Posix.Types
