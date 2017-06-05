{-# LANGUAGE Haskell2010, CPP, Rank2Types, DeriveDataTypeable, StandaloneDeriving #-}
{-# LINE 1 "lib/Data/Time/Clock/POSIX.hs" #-}

















































-- | POSIX time, if you need to deal with timestamps and the like.
-- Most people won't need this module.
module Data.Time.Clock.POSIX
(
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime
) where

import Data.Time.Clock.UTC
import Data.Time.Calendar.Days
import Data.Fixed
import Control.Monad















































































import Data.Time.Clock.CTimespec
import Foreign.C.Types (CTime(..))

-- | 86400 nominal seconds in every day
posixDayLength :: NominalDiffTime
posixDayLength = 86400

-- | POSIX time is the nominal time since 1970-01-01 00:00 UTC
--
-- To convert from a 'Foreign.C.CTime' or 'System.Posix.EpochTime', use 'realToFrac'.
--
type POSIXTime = NominalDiffTime

unixEpochDay :: Day
unixEpochDay = ModifiedJulianDay 40587

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime i = let
    (d,t) = divMod' i posixDayLength
 in UTCTime (addDays d unixEpochDay) (realToFrac t)

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime d t) =
 (fromInteger (diffDays d unixEpochDay) * posixDayLength) + min posixDayLength (realToFrac t)

-- | Get the current POSIX time from the system clock.
getPOSIXTime :: IO POSIXTime


-- Use hi-res POSIX time
ctimespecToPosixSeconds :: CTimespec -> POSIXTime
ctimespecToPosixSeconds (MkCTimespec (CTime s) ns) =
    (fromIntegral s) + (fromIntegral ns) / 1000000000

getPOSIXTime = liftM ctimespecToPosixSeconds getCTimespec


-- | Get the current UTC time from the system clock.
getCurrentTime :: IO UTCTime
getCurrentTime = liftM posixSecondsToUTCTime getPOSIXTime
