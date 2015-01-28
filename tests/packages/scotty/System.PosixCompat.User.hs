{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/PosixCompat/User.hs" #-}











































{-# LINE 1 "src/System/PosixCompat/User.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "src/System/PosixCompat/User.hsc" #-}

{-|
This module makes the operations exported by @System.Posix.User@
available on all platforms. On POSIX systems it re-exports operations from
@System.Posix.User@. On other platforms it provides dummy implementations.
-}
module System.PosixCompat.User (
    -- * User environment
    -- ** Querying the user environment
      getRealUserID
    , getRealGroupID
    , getEffectiveUserID
    , getEffectiveGroupID
    , getGroups
    , getLoginName
    , getEffectiveUserName

    -- *** The group database
    , GroupEntry(..)
    , getGroupEntryForID
    , getGroupEntryForName
    , getAllGroupEntries

    -- *** The user database
    , UserEntry(..)
    , getUserEntryForID
    , getUserEntryForName
    , getAllUserEntries

    -- ** Modifying the user environment
    , setUserID
    , setGroupID
    ) where


{-# LINE 37 "src/System/PosixCompat/User.hsc" #-}


{-# LINE 39 "src/System/PosixCompat/User.hsc" #-}

import System.Posix.User


{-# LINE 49 "src/System/PosixCompat/User.hsc" #-}


{-# LINE 134 "src/System/PosixCompat/User.hsc" #-}
