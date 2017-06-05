{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Time.hs" #-}
{-# LINE 1 "System/Time.hsc" #-}

{-# LINE 2 "System/Time.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 4 "System/Time.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Time
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/old-time/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard time library from Haskell 98.  This library is
-- deprecated, please look at @Data.Time@ in the @time@ package
-- instead.
--
-- "System.Time" provides functionality for clock times, including
-- timezone information (i.e, the functionality of \"@time.h@\",
-- adapted to the Haskell environment).  It follows RFC 1129 in its
-- use of Coordinated Universal Time (UTC).
--
-----------------------------------------------------------------------------

{-
Haskell 98 Time of Day Library
------------------------------

2000/06/17 <michael.weber@post.rwth-aachen.de>:
RESTRICTIONS:
  * min./max. time diff currently is restricted to
    [minBound::Int, maxBound::Int]

  * surely other restrictions wrt. min/max bounds


NOTES:
  * printing times

    `showTime' (used in `instance Show ClockTime') always prints time
    converted to the local timezone (even if it is taken from
    `(toClockTime . toUTCTime)'), whereas `calendarTimeToString'
    honors the tzone & tz fields and prints UTC or whatever timezone
    is stored inside CalendarTime.

    Maybe `showTime' should be changed to use UTC, since it would
    better correspond to the actual representation of `ClockTime'
    (can be done by replacing localtime(3) by gmtime(3)).


BUGS:
  * add proper handling of microsecs, currently, they're mostly
    ignored

  * `formatFOO' case of `%s' is currently broken...


TODO:
  * check for unusual date cases, like 1970/1/1 00:00h, and conversions
    between different timezone's etc.

  * check, what needs to be in the IO monad, the current situation
    seems to be a bit inconsistent to me

  * check whether `isDst = -1' works as expected on other arch's
    (Solaris anyone?)

  * add functions to parse strings to `CalendarTime' (some day...)

  * implement padding capabilities ("%_", "%-") in `formatFOO'

  * add rfc822 timezone (+0200 is CEST) representation ("%z") in `formatFOO'
-}

module System.Time
     (
        -- * Clock times

        ClockTime(..) -- non-standard, lib. report gives this as abstract
        -- instance Eq, Ord
        -- instance Show (non-standard)

     ,  getClockTime

        -- * Time differences

     ,  TimeDiff(..)
     ,  noTimeDiff      -- non-standard (but useful when constructing TimeDiff vals.)
     ,  diffClockTimes
     ,  addToClockTime

     ,  normalizeTimeDiff -- non-standard
     ,  timeDiffToString  -- non-standard
     ,  formatTimeDiff    -- non-standard

        -- * Calendar times

     ,  CalendarTime(..)
     ,  Month(..)
     ,  Day(..)
     ,  toCalendarTime
     ,  toUTCTime
     ,  toClockTime
     ,  calendarTimeToString
     ,  formatCalendarTime

     ) where


{-# LINE 111 "System/Time.hsc" #-}

{-# LINE 112 "System/Time.hsc" #-}

{-# LINE 113 "System/Time.hsc" #-}

import Prelude

import Data.Ix
import System.Locale
import Foreign
import System.IO.Unsafe (unsafePerformIO)


{-# LINE 124 "System/Time.hsc" #-}
import Foreign.C

{-# LINE 126 "System/Time.hsc" #-}

-- One way to partition and give name to chunks of a year and a week:

-- | A month of the year.

data Month
 = January   | February | March    | April
 | May       | June     | July     | August
 | September | October  | November | December
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- | A day of the week.

data Day
 = Sunday   | Monday | Tuesday | Wednesday
 | Thursday | Friday | Saturday
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- | A representation of the internal clock time.
-- Clock times may be compared, converted to strings, or converted to an
-- external calendar time 'CalendarTime' for I\/O or other manipulations.

data ClockTime = TOD Integer Integer
                -- ^ Construct a clock time.  The arguments are a number
                -- of seconds since 00:00:00 (UTC) on 1 January 1970,
                -- and an additional number of picoseconds.
                --
                -- In Haskell 98, the 'ClockTime' type is abstract.
               deriving (Eq, Ord)

-- When a ClockTime is shown, it is converted to a CalendarTime in the current
-- timezone and then printed.  FIXME: This is arguably wrong, since we can't
-- get the current timezone without being in the IO monad.

instance Show ClockTime where
    showsPrec _ t = showString (calendarTimeToString
                                 (unsafePerformIO (toCalendarTime t)))

{-
The numeric fields have the following ranges.

\begin{verbatim}
Value         Range             Comments
-----         -----             --------

year    -maxInt .. maxInt       [Pre-Gregorian dates are inaccurate]
day           1 .. 31
hour          0 .. 23
min           0 .. 59
sec           0 .. 61           [Allows for two leap seconds]
picosec       0 .. (10^12)-1    [This could be over-precise?]
yday          0 .. 365          [364 in non-Leap years]
tz       -43200 .. 50400        [Variation from UTC in seconds]
\end{verbatim}
-}

-- | 'CalendarTime' is a user-readable and manipulable
-- representation of the internal 'ClockTime' type.

data CalendarTime
 = CalendarTime  {
       ctYear    :: Int         -- ^ Year (pre-Gregorian dates are inaccurate)
     , ctMonth   :: Month       -- ^ Month of the year
     , ctDay     :: Int         -- ^ Day of the month (1 to 31)
     , ctHour    :: Int         -- ^ Hour of the day (0 to 23)
     , ctMin     :: Int         -- ^ Minutes (0 to 59)
     , ctSec     :: Int         -- ^ Seconds (0 to 61, allowing for up to
                                -- two leap seconds)
     , ctPicosec :: Integer     -- ^ Picoseconds
     , ctWDay    :: Day         -- ^ Day of the week
     , ctYDay    :: Int         -- ^ Day of the year
                                -- (0 to 364, or 365 in leap years)
     , ctTZName  :: String      -- ^ Name of the time zone
     , ctTZ      :: Int         -- ^ Variation from UTC in seconds
     , ctIsDST   :: Bool        -- ^ 'True' if Daylight Savings Time would
                                -- be in effect, and 'False' otherwise
 }
 deriving (Eq,Ord,Read,Show)

-- | records the difference between two clock times in a user-readable way.

data TimeDiff
 = TimeDiff {
     tdYear    :: Int,
     tdMonth   :: Int,
     tdDay     :: Int,
     tdHour    :: Int,
     tdMin     :: Int,
     tdSec     :: Int,
     tdPicosec :: Integer -- not standard
   }
   deriving (Eq,Ord,Read,Show)

-- | null time difference.

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0

-- -----------------------------------------------------------------------------
-- | returns the current time in its internal representation.

realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)
  -- CTime, CClock, CUShort etc are in Real but not Fractional,
  -- so we must convert to Double before we can round it

getClockTime :: IO ClockTime

{-# LINE 239 "System/Time.hsc" #-}


{-# LINE 244 "System/Time.hsc" #-}
type Timeval_tv_sec = CTime
type Timeval_tv_usec = CSUSeconds

{-# LINE 247 "System/Time.hsc" #-}

getClockTime = do
  allocaBytes (16) $ \ p_timeval -> do
{-# LINE 250 "System/Time.hsc" #-}
    throwErrnoIfMinus1_ "getClockTime" $ gettimeofday p_timeval nullPtr
    sec  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p_timeval :: IO Timeval_tv_sec
{-# LINE 252 "System/Time.hsc" #-}
    usec <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_timeval :: IO Timeval_tv_usec
{-# LINE 253 "System/Time.hsc" #-}
    return (TOD (realToInteger sec) ((realToInteger usec) * 1000000))


{-# LINE 269 "System/Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- | @'addToClockTime' d t@ adds a time difference @d@ and a
-- clock time @t@ to yield a new clock time.  The difference @d@
-- may be either positive or negative.

addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour minute sec psec)
               (TOD c_sec c_psec) =
        let
          sec_diff = toInteger sec +
                     60 * toInteger minute +
                     3600 * toInteger hour +
                     24 * 3600 * toInteger day
          (d_sec, d_psec) = (c_psec + psec) `quotRem` 1000000000000
          cal      = toUTCTime (TOD (c_sec + sec_diff + d_sec) d_psec)
          new_mon  = fromEnum (ctMonth cal) + r_mon
          month' = fst tmp
          yr_diff = snd tmp
          tmp
            | new_mon < 0  = (toEnum (12 + new_mon), (-1))
            | new_mon > 11 = (toEnum (new_mon `mod` 12), 1)
            | otherwise    = (toEnum new_mon, 0)

          (r_yr, r_mon) = mon `quotRem` 12

          year' = ctYear cal + year + r_yr + yr_diff
        in
        toClockTime cal{ctMonth=month', ctYear=year'}

-- | @'diffClockTimes' t1 t2@ returns the difference between two clock
-- times @t1@ and @t2@ as a 'TimeDiff'.

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
-- diffClockTimes is meant to be the dual to `addToClockTime'.
-- If you want to have the TimeDiff properly splitted, use
-- `normalizeTimeDiff' on this function's result
--
-- CAVEAT: see comment of normalizeTimeDiff
diffClockTimes (TOD sa pa) (TOD sb pb) =
    noTimeDiff{ tdSec     = fromIntegral (sa - sb)
                -- FIXME: can handle just 68 years...
              , tdPicosec = pa - pb
              }


-- | converts a time difference to normal form.

normalizeTimeDiff :: TimeDiff -> TimeDiff
-- FIXME: handle psecs properly
-- FIXME: ?should be called by formatTimeDiff automagically?
--
-- when applied to something coming out of `diffClockTimes', you loose
-- the duality to `addToClockTime', since a year does not always have
-- 365 days, etc.
--
-- apply this function as late as possible to prevent those "rounding"
-- errors
normalizeTimeDiff td =
  let
      rest0 = toInteger (tdSec td)
               + 60 * (toInteger (tdMin td)
                    + 60 * (toInteger (tdHour td)
                         + 24 * (toInteger (tdDay td)
                              + 30 * toInteger (tdMonth td)
                              + 365 * toInteger (tdYear td))))

      (diffYears,  rest1)    = rest0 `quotRem` (365 * 24 * 3600)
      (diffMonths, rest2)    = rest1 `quotRem` (30 * 24 * 3600)
      (diffDays,   rest3)    = rest2 `quotRem` (24 * 3600)
      (diffHours,  rest4)    = rest3 `quotRem` 3600
      (diffMins,   diffSecs) = rest4 `quotRem` 60
  in
      td{ tdYear  = fromInteger diffYears
        , tdMonth = fromInteger diffMonths
        , tdDay   = fromInteger diffDays
        , tdHour  = fromInteger diffHours
        , tdMin   = fromInteger diffMins
        , tdSec   = fromInteger diffSecs
        }


{-# LINE 351 "System/Time.hsc" #-}
-- -----------------------------------------------------------------------------
-- How do we deal with timezones on this architecture?

-- The POSIX way to do it is through the global variable tzname[].
-- But that's crap, so we do it The BSD Way if we can: namely use the
-- tm_zone and tm_gmtoff fields of struct tm, if they're available.

zone   :: Ptr CTm -> IO (Ptr CChar)
gmtoff :: Ptr CTm -> IO CLong

{-# LINE 361 "System/Time.hsc" #-}
zone x      = ((\hsc_ptr -> peekByteOff hsc_ptr 48)) x
{-# LINE 362 "System/Time.hsc" #-}
gmtoff x    = ((\hsc_ptr -> peekByteOff hsc_ptr 40)) x
{-# LINE 363 "System/Time.hsc" #-}


{-# LINE 410 "System/Time.hsc" #-}

{-# LINE 411 "System/Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- | converts an internal clock time to a local time, modified by the
-- timezone and daylight savings time settings in force at the time
-- of conversion.  Because of this dependence on the local environment,
-- 'toCalendarTime' is in the 'IO' monad.

toCalendarTime :: ClockTime -> IO CalendarTime

{-# LINE 422 "System/Time.hsc" #-}
toCalendarTime =  clockToCalendarTime_reentrant (_throwAwayReturnPointer localtime_r) False

{-# LINE 426 "System/Time.hsc" #-}

-- | converts an internal clock time into a 'CalendarTime' in standard
-- UTC format.

toUTCTime :: ClockTime -> CalendarTime

{-# LINE 434 "System/Time.hsc" #-}
toUTCTime      =  unsafePerformIO . clockToCalendarTime_reentrant (_throwAwayReturnPointer gmtime_r) True

{-# LINE 438 "System/Time.hsc" #-}


{-# LINE 463 "System/Time.hsc" #-}
_throwAwayReturnPointer :: (Ptr CTime -> Ptr CTm -> IO (Ptr CTm))
                        -> (Ptr CTime -> Ptr CTm -> IO (       ))
_throwAwayReturnPointer fun x y = fun x y >> return ()


{-# LINE 475 "System/Time.hsc" #-}


{-# LINE 477 "System/Time.hsc" #-}
clockToCalendarTime_reentrant :: (Ptr CTime -> Ptr CTm -> IO ()) -> Bool -> ClockTime
         -> IO CalendarTime
clockToCalendarTime_reentrant fun is_utc (TOD secs psec) = do
  with (fromIntegral secs :: CTime)  $ \ p_timer -> do
    allocaBytes (56) $ \ p_tm -> do
{-# LINE 482 "System/Time.hsc" #-}
      fun p_timer p_tm
      clockToCalendarTime_aux is_utc p_tm psec

{-# LINE 485 "System/Time.hsc" #-}

clockToCalendarTime_aux :: Bool -> Ptr CTm -> Integer -> IO CalendarTime
clockToCalendarTime_aux is_utc p_tm psec = do
    sec   <-  ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p_tm :: IO CInt
{-# LINE 489 "System/Time.hsc" #-}
    minute <-  ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p_tm :: IO CInt
{-# LINE 490 "System/Time.hsc" #-}
    hour  <-  ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p_tm :: IO CInt
{-# LINE 491 "System/Time.hsc" #-}
    mday  <-  ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p_tm :: IO CInt
{-# LINE 492 "System/Time.hsc" #-}
    mon   <-  ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p_tm :: IO CInt
{-# LINE 493 "System/Time.hsc" #-}
    year  <-  ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p_tm :: IO CInt
{-# LINE 494 "System/Time.hsc" #-}
    wday  <-  ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p_tm :: IO CInt
{-# LINE 495 "System/Time.hsc" #-}
    yday  <-  ((\hsc_ptr -> peekByteOff hsc_ptr 28)) p_tm :: IO CInt
{-# LINE 496 "System/Time.hsc" #-}
    isdst <-  ((\hsc_ptr -> peekByteOff hsc_ptr 32)) p_tm :: IO CInt
{-# LINE 497 "System/Time.hsc" #-}
    zone' <-  zone p_tm
    tz    <-  gmtoff p_tm

    tzname' <- peekCString zone'

    let month  | mon >= 0 && mon <= 11 = toEnum (fromIntegral mon)
               | otherwise             = error ("toCalendarTime: illegal month value: " ++ show mon)

    return (CalendarTime
                (1900 + fromIntegral year)
                month
                (fromIntegral mday)
                (fromIntegral hour)
                (fromIntegral minute)
                (fromIntegral sec)
                psec
                (toEnum (fromIntegral wday))
                (fromIntegral yday)
                (if is_utc then "UTC" else tzname')
                (if is_utc then 0     else fromIntegral tz)
                (if is_utc then False else isdst /= 0))

{-# LINE 519 "System/Time.hsc" #-}

-- | converts a 'CalendarTime' into the corresponding internal
-- 'ClockTime', ignoring the contents of the  'ctWDay', 'ctYDay',
-- 'ctTZName' and 'ctIsDST' fields.

toClockTime :: CalendarTime -> ClockTime

{-# LINE 532 "System/Time.hsc" #-}
toClockTime (CalendarTime year mon mday hour minute sec psec
                          _wday _yday _tzname tz _isdst) =

     -- `isDst' causes the date to be wrong by one hour...
     -- FIXME: check, whether this works on other arch's than Linux, too...
     --
     -- so we set it to (-1) (means `unknown') and let `mktime' determine
     -- the real value...
    let isDst = -1 :: CInt in   -- if _isdst then (1::Int) else 0

    if psec < 0 || psec > 999999999999 then
        error "Time.toClockTime: picoseconds out of range"
    else if tz < -43200 || tz > 50400 then
        error "Time.toClockTime: timezone offset out of range"
    else
      unsafePerformIO $ do
      allocaBytes (56) $ \ p_tm -> do
{-# LINE 549 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_tm (fromIntegral sec  :: CInt)
{-# LINE 550 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p_tm (fromIntegral minute :: CInt)
{-# LINE 551 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p_tm (fromIntegral hour :: CInt)
{-# LINE 552 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p_tm (fromIntegral mday :: CInt)
{-# LINE 553 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p_tm (fromIntegral (fromEnum mon) :: CInt)
{-# LINE 554 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) p_tm (fromIntegral year - 1900 :: CInt)
{-# LINE 555 "System/Time.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) p_tm isDst
{-# LINE 556 "System/Time.hsc" #-}
        t <- throwIf (== -1) (\_ -> "Time.toClockTime: invalid input")
                (mktime p_tm)
        --
        -- mktime expects its argument to be in the local timezone, but
        -- toUTCTime makes UTC-encoded CalendarTime's ...
        --
        -- Since there is no any_tz_struct_tm-to-time_t conversion
        -- function, we have to fake one... :-) If not in all, it works in
        -- most cases (before, it was the other way round...)
        --
        -- Luckily, mktime tells us, what it *thinks* the timezone is, so,
        -- to compensate, we add the timezone difference to mktime's
        -- result.
        --
        gmtoffset <- gmtoff p_tm
        let res = realToInteger t - fromIntegral tz + fromIntegral gmtoffset
        return (TOD res psec)

{-# LINE 574 "System/Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- Converting time values to strings.

-- | formats calendar times using local conventions.

calendarTimeToString  :: CalendarTime -> String
calendarTimeToString  =  formatCalendarTime defaultTimeLocale "%c"

-- | formats calendar times using local conventions and a formatting string.
-- The formatting string is that understood by the ISO C @strftime()@
-- function.

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt cal@(CalendarTime year mon day hour minute sec _
                                       wday yday tzname' _ _) =
        doFmt fmt
  where doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':c:cs)   = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        decode 'A' = fst (wDays l  !! fromEnum wday) -- day of the week, full name
        decode 'a' = snd (wDays l  !! fromEnum wday) -- day of the week, abbrev.
        decode 'B' = fst (months l !! fromEnum mon)  -- month, full name
        decode 'b' = snd (months l !! fromEnum mon)  -- month, abbrev
        decode 'h' = snd (months l !! fromEnum mon)  -- ditto
        decode 'C' = show2 (year `quot` 100)         -- century
        decode 'c' = doFmt (dateTimeFmt l)           -- locale's data and time format.
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day                       -- day of the month
        decode 'e' = show2' day                      -- ditto, padded
        decode 'H' = show2 hour                      -- hours, 24-hour clock, padded
        decode 'I' = show2 (to12 hour)               -- hours, 12-hour clock
        decode 'j' = show3 (yday + 1)                -- day of the year
        decode 'k' = show2' hour                     -- hours, 24-hour clock, no padding
        decode 'l' = show2' (to12 hour)              -- hours, 12-hour clock, no padding
        decode 'M' = show2 minute                    -- minutes
        decode 'm' = show2 (fromEnum mon+1)          -- numeric month
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l) -- am or pm
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec                       -- seconds
        decode 's' = let TOD esecs _ = toClockTime cal in show esecs
                                                     -- number of secs since Epoch.
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7) -- week number, starting on Sunday.
        decode 'u' = show (let n = fromEnum wday in  -- numeric day of the week (1=Monday, 7=Sunday)
                           if n == 0 then 7 else n)
        decode 'V' =                                 -- week number (as per ISO-8601.)
            let (week, days) =                       -- [yep, I've always wanted to be able to display that too.]
                   (yday + 7 - if fromEnum wday > 0 then
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1
                       else if week == 0 then 53 else week)

        decode 'W' =                                 -- week number, weeks starting on monday
            show2 ((yday + 7 - if fromEnum wday > 0 then
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)            -- numeric day of the week, weeks starting on Sunday.
        decode 'X' = doFmt (timeFmt l)               -- locale's preferred way of printing time.
        decode 'x' = doFmt (dateFmt l)               -- locale's preferred way of printing dates.
        decode 'Y' = show year                       -- year, including century.
        decode 'y' = show2 (year `rem` 100)          -- year, within century.
        decode 'Z' = tzname'                         -- timezone name
        decode '%' = "%"
        decode c   = [c]


show2, show2', show3 :: Int -> String
show2 x
 | x' < 10   = '0': show x'
 | otherwise = show x'
 where x' = x `rem` 100

show2' x
 | x' < 10   = ' ': show x'
 | otherwise = show x'
 where x' = x `rem` 100

show3 x = show (x `quot` 100) ++ show2 (x `rem` 100)

to12 :: Int -> Int
to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'

-- Useful extensions for formatting TimeDiffs.

-- | formats time differences using local conventions.

timeDiffToString :: TimeDiff -> String
timeDiffToString = formatTimeDiff defaultTimeLocale "%c"

-- | formats time differences using local conventions and a formatting string.
-- The formatting string is that understood by the ISO C @strftime()@
-- function.

formatTimeDiff :: TimeLocale -> String -> TimeDiff -> String
formatTimeDiff l fmt (TimeDiff year month day hour minute sec _)
 = doFmt fmt
  where
   doFmt ""         = ""
   doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':c:cs) = decode c ++ doFmt cs
   doFmt (c:cs)     = c : doFmt cs

   decode spec =
    case spec of
      'B' -> fst (months l !! fromEnum month)
      'b' -> snd (months l !! fromEnum month)
      'h' -> snd (months l !! fromEnum month)
      'c' -> defaultTimeDiffFmt
      'C' -> show2 (year `quot` 100)
      'D' -> doFmt "%m/%d/%y"
      'd' -> show2 day
      'e' -> show2' day
      'H' -> show2 hour
      'I' -> show2 (to12 hour)
      'k' -> show2' hour
      'l' -> show2' (to12 hour)
      'M' -> show2 minute
      'm' -> show2 (fromEnum month + 1)
      'n' -> "\n"
      'p' -> (if hour < 12 then fst else snd) (amPm l)
      'R' -> doFmt "%H:%M"
      'r' -> doFmt (time12Fmt l)
      'T' -> doFmt "%H:%M:%S"
      't' -> "\t"
      'S' -> show2 sec
      's' -> show2 sec -- Implementation-dependent, sez the lib doc..
      'X' -> doFmt (timeFmt l)
      'x' -> doFmt (dateFmt l)
      'Y' -> show year
      'y' -> show2 (year `rem` 100)
      '%' -> "%"
      c   -> [c]

   defaultTimeDiffFmt =
       foldr (\ (v,s) rest ->
                  (if v /= 0
                     then show v ++ ' ':(addS v s)
                       ++ if null rest then "" else ", "
                     else "") ++ rest
             )
             ""
             (zip [year, month, day, hour, minute, sec] (intervals l))

   addS v s = if abs v == 1 then fst s else snd s


{-# LINE 728 "System/Time.hsc" #-}
-- -----------------------------------------------------------------------------
-- Foreign time interface (POSIX)

type CTm = () -- struct tm


{-# LINE 734 "System/Time.hsc" #-}
foreign import ccall unsafe "HsTime.h __hscore_localtime_r"
    localtime_r :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

{-# LINE 740 "System/Time.hsc" #-}

{-# LINE 741 "System/Time.hsc" #-}
foreign import ccall unsafe "HsTime.h __hscore_gmtime_r"
    gmtime_r    :: Ptr CTime -> Ptr CTm -> IO (Ptr CTm)

{-# LINE 747 "System/Time.hsc" #-}
foreign import ccall unsafe "time.h mktime"
    mktime      :: Ptr CTm   -> IO CTime


{-# LINE 751 "System/Time.hsc" #-}
type CTimeVal = ()
type CTimeZone = ()
foreign import ccall unsafe "HsTime.h __hscore_gettimeofday"
    gettimeofday :: Ptr CTimeVal -> Ptr CTimeZone -> IO CInt

{-# LINE 765 "System/Time.hsc" #-}

{-# LINE 766 "System/Time.hsc" #-}
