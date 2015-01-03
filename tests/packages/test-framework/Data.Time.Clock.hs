{-# LINE 1 "lib/Data/Time/Clock.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                             






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "lib/Data/Time/Clock.hs" #-}
{-# LINE 1 "lib/Data/Time/Clock.hs" #-}
-- | Types and functions for UTC and UT1
module Data.Time.Clock
(
	module Data.Time.Clock.Scale,
	module Data.Time.Clock.UTC,
	module Data.Time.Clock.UTCDiff,
	module Data.Time.Clock
) where

import Data.Time.Clock.Scale
import Data.Time.Clock.UTCDiff
import Data.Time.Clock.UTC
import Data.Time.Clock.POSIX
import Control.Monad

-- | Get the current UTC time from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = liftM posixSecondsToUTCTime getPOSIXTime
