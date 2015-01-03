{-# LINE 1 "lib/Data/Time/Format/Parse.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                             






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "lib/Data/Time/Format/Parse.hs" #-}
{-# LINE 1 "lib/Data/Time/Format/Parse.hs" #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LINE 1 "lib/include/HsConfigure.h" #-}








{-# LINE 3 "lib/Data/Time/Format/Parse.hs" #-}

-- #hide
module Data.Time.Format.Parse
    (
    -- * UNIX-style parsing




    ParseTime(..),
    -- * Locale
    module Data.Time.Format.Locale
    ) where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime




import Data.Char
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Time.Format.Locale
















-- | Convert string to upper case.
up :: String -> String
up = map toUpper


-- | The class of types which can be parsed given a UNIX-style time format
-- string.
class ParseTime t where
    -- | Builds a time value from a parsed input string.
    -- If the input does not include all the information needed to
    -- construct a complete value, any missing parts should be taken
    -- from 1970-01-01 00:00:00 +0000 (which was a Thursday).
    -- In the absence of @%C@ or @%Y@, century is 1969 - 2068.
    buildTime :: TimeLocale -- ^ The time locale.
              -> [(Char,String)] -- ^ Pairs of format characters and the
                                 -- corresponding part of the input.
              -> t














































































































































































































































--
-- * Instances for the time package types
--

data DayComponent = Century Integer -- century of all years
                  | CenturyYear Integer -- 0-99, last two digits of both real years and week years
                  | YearMonth Int -- 1-12
                  | MonthDay Int -- 1-31
                  | YearDay Int -- 1-366
                  | WeekDay Int -- 1-7 (mon-sun)
                  | YearWeek WeekType Int -- 1-53 or 0-53

data WeekType = ISOWeek | SundayWeek | MondayWeek

instance ParseTime Day where
    buildTime l = buildDay . concatMap (uncurry f)
     where
      f c x =
        case c of
          -- %C: century (all but the last two digits of the year), 00 - 99
          'C' -> [Century (read x)]
          -- %f century (all but the last two digits of the year), 00 - 99
          'f' -> [Century (read x)]
          -- %Y: year
          'Y' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %G: year for Week Date format
          'G' -> let y = read x in [Century (y `div` 100), CenturyYear (y `mod` 100)]
          -- %y: last two digits of year, 00 - 99
          'y' -> [CenturyYear (read x)]
          -- %g: last two digits of year for Week Date format, 00 - 99
          'g' -> [CenturyYear (read x)]
          -- %B: month name, long form (fst from months locale), January - December
          'B' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . fst) (months l))))]
          -- %b: month name, short form (snd from months locale), Jan - Dec
          'b' -> [YearMonth (1 + fromJust (elemIndex (up x) (map (up . snd) (months l))))]
          -- %m: month of year, leading 0 as needed, 01 - 12
          'm' -> [YearMonth (read x)]
          -- %d: day of month, leading 0 as needed, 01 - 31
          'd' -> [MonthDay (read x)]
          -- %e: day of month, leading space as needed, 1 - 31
          'e' -> [MonthDay (read x)]
          -- %V: week for Week Date format, 01 - 53
          'V' -> [YearWeek ISOWeek (read x)]
          -- %U: week number of year, where weeks start on Sunday (as sundayStartWeek), 01 - 53
          'U' -> [YearWeek SundayWeek (read x)]
          -- %W: week number of year, where weeks start on Monday (as mondayStartWeek), 01 - 53
          'W' -> [YearWeek MondayWeek (read x)]
          -- %u: day for Week Date format, 1 - 7
          'u' -> [WeekDay (read x)]
          -- %a: day of week, short form (snd from wDays locale), Sun - Sat
          'a' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . snd) (wDays l))) + 6) `mod` 7)]
          -- %A: day of week, long form (fst from wDays locale), Sunday - Saturday
          'A' -> [WeekDay (1 + (fromJust (elemIndex (up x) (map (up . fst) (wDays l))) + 6) `mod` 7)]
          -- %w: day of week number, 0 (= Sunday) - 6 (= Saturday)
          'w' -> [WeekDay (((read x + 6) `mod` 7) + 1)]
          -- %j: day of year for Ordinal Date format, 001 - 366
          'j' -> [YearDay (read x)]
          _   -> []

      buildDay cs = rest cs
        where
        y = let
                d = safeLast 70 [x | CenturyYear x <- cs]
                c = safeLast (if d >= 69 then 19 else 20) [x | Century x <- cs]
             in 100 * c + d

        rest (YearMonth m:_)  = let d = safeLast 1 [x | MonthDay x <- cs]
                             in fromGregorian y m d
        rest (YearDay d:_) = fromOrdinalDate y d
        rest (YearWeek wt w:_) = let d = safeLast 4 [x | WeekDay x <- cs]
                              in case wt of
                                   ISOWeek    -> fromWeekDate y w d
                                   SundayWeek -> fromSundayStartWeek y w (d `mod` 7)
                                   MondayWeek -> fromMondayStartWeek y w d
        rest (_:xs)        = rest xs
        rest []            = rest [YearMonth 1]

      safeLast x xs = last (x:xs)

instance ParseTime TimeOfDay where
    buildTime l = foldl f midnight
        where
          f t@(TimeOfDay h m s) (c,x) =
              case c of
                'P' -> if up x == fst (amPm l) then am else pm
                'p' -> if up x == fst (amPm l) then am else pm
                'H' -> TimeOfDay (read x) m s
                'I' -> TimeOfDay (read x) m s
                'k' -> TimeOfDay (read x) m s
                'l' -> TimeOfDay (read x) m s
                'M' -> TimeOfDay h (read x) s
                'S' -> TimeOfDay h m (fromInteger (read x))
                'q' -> TimeOfDay h m (mkPico (truncate s) (read x))
                'Q' -> if null x then t
                        else let ps = read $ take 12 $ rpad 12 '0' $ drop 1 x
                              in TimeOfDay h m (mkPico (truncate s) ps)
                _   -> t
            where am = TimeOfDay (h `mod` 12) m s
                  pm = TimeOfDay (if h < 12 then h + 12 else h) m s

rpad :: Int -> a -> [a] -> [a]
rpad n c xs = xs ++ replicate (n - length xs) c

mkPico :: Integer -> Integer -> Pico
mkPico i f = fromInteger i + fromRational (f % 1000000000000)

instance ParseTime LocalTime where
    buildTime l xs = LocalTime (buildTime l xs) (buildTime l xs)

enumDiff :: (Enum a) => a -> a -> Int
enumDiff a b = (fromEnum a) - (fromEnum b)

getMilZoneHours :: Char -> Maybe Int
getMilZoneHours c | c < 'A' = Nothing
getMilZoneHours c | c <= 'I' = Just $ 1 + enumDiff c 'A'
getMilZoneHours 'J' = Nothing
getMilZoneHours c | c <= 'M' = Just $ 10 + enumDiff c 'K'
getMilZoneHours c | c <= 'Y' = Just $ (enumDiff 'N' c) - 1
getMilZoneHours 'Z' = Just 0
getMilZoneHours _ = Nothing

instance ParseTime TimeZone where
    buildTime l = foldl f (minutesToTimeZone 0)
      where
        f t@(TimeZone offset dst name) (c,x) =
            case c of
              'z' -> zone
              'Z' | null x           -> t
                  | isAlpha (head x) -> let y = up x in
                      case find (\tz -> y == timeZoneName tz) (knownTimeZones l) of
                        Just tz -> tz
                        Nothing -> case y of
                            [yc] | Just hours <- getMilZoneHours yc -> TimeZone (hours * 60) False y
                            _ -> TimeZone offset dst y
                  | otherwise        -> zone
              _   -> t
          where zone = TimeZone (readTzOffset x) dst name

readTzOffset :: String -> Int
readTzOffset str =
    case str of
      (s:h1:h2:':':m1:m2:[]) -> calc s h1 h2 m1 m2
      (s:h1:h2:m1:m2:[]) -> calc s h1 h2 m1 m2
      _ -> 0
    where calc s h1 h2 m1 m2 = sign * (60 * h + m)
              where sign = if s == '-' then -1 else 1
                    h = read [h1,h2]
                    m = read [m1,m2]

instance ParseTime ZonedTime where
    buildTime l xs = foldl f (ZonedTime (buildTime l xs) (buildTime l xs)) xs
        where
          f t@(ZonedTime (LocalTime _ tod) z) (c,x) =
              case c of
                's' -> let s = fromInteger (read x)
                           (_,ps) = properFraction (todSec tod) :: (Integer,Pico)
                           s' = s + fromRational (toRational ps)
                        in utcToZonedTime z (posixSecondsToUTCTime s')
                _   -> t

instance ParseTime UTCTime where
    buildTime l = zonedTimeToUTC . buildTime l

-- * Read instances for time package types






















