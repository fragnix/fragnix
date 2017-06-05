{-# LANGUAGE Haskell2010, CPP, Rank2Types, DeriveDataTypeable, StandaloneDeriving #-}
{-# LINE 1 "lib/Data/Time/Clock/UTCDiff.hs" #-}

















































-- #hide
module Data.Time.Clock.UTCDiff where

import Data.Time.Clock.POSIX
import Data.Time.Clock.UTC

-- | addUTCTime a b = a + b
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime x t = posixSecondsToUTCTime (x + (utcTimeToPOSIXSeconds t))

-- | diffUTCTime a b = a - b
diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b = (utcTimeToPOSIXSeconds a) - (utcTimeToPOSIXSeconds b)
