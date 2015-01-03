{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Time.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Time.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Time.hs" #-}
{-# LINE 1 "System/Posix/Time.hsc" #-}

{-# LINE 4 "System/Posix/Time.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Time
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Time support
--
-----------------------------------------------------------------------------

module System.Posix.Time (
	epochTime,
	-- ToDo: lots more from sys/time.h
	-- how much already supported by System.Time?
  ) where


{-# LINE 25 "System/Posix/Time.hsc" #-}

import System.Posix.Types
import Foreign
import Foreign.C

-- -----------------------------------------------------------------------------
-- epochTime

-- | @epochTime@ calls @time@ to obtain the number of 
--   seconds that have elapsed since the epoch (Jan 01 00:00:00 GMT 1970).
epochTime :: IO EpochTime
epochTime = throwErrnoIfMinus1 "epochTime" (c_time nullPtr)

foreign import ccall unsafe "__hsunix_time"
  c_time :: Ptr CTime -> IO CTime
