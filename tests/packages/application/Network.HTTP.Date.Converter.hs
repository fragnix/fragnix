{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Date/Converter.hs" #-}
{-# LANGUAGE BangPatterns #-}
module Network.HTTP.Date.Converter (epochTimeToHTTPDate) where

import Control.Applicative
import Data.ByteString.Internal
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Network.HTTP.Date.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types

{-|
  Translating 'EpochTime' to 'HTTPDate'.
-}
epochTimeToHTTPDate :: EpochTime -> HTTPDate
epochTimeToHTTPDate x = defaultHTTPDate {
    hdYear   = y
  , hdMonth  = m
  , hdDay    = d
  , hdHour   = h
  , hdMinute = n
  , hdSecond = s
  , hdWkday  = w
  }
  where
    w64 :: Word64
    w64 = fromIntegral $ fromEnum x
    (days',secs') = w64 `quotRem` 86400
    days = fromIntegral days'
    secs = fromIntegral secs'
    -- 1970/1/1 is Thu (4)
    w = (days + 3) `rem` 7 + 1
    (y,m,d) = toYYMMDD days
    (h,n,s) = toHHMMSS secs

toYYMMDD :: Int -> (Int,Int,Int)
toYYMMDD x = (yy, mm, dd)
  where
    (y,d) = x `quotRem` 365
    cy = 1970 + y
    cy' = cy - 1
    leap = cy' `quot` 4 - cy' `quot` 100 + cy' `quot` 400 - 477
    (yy,days) = adjust cy d leap
    (mm,dd) = findMonth days
    adjust !ty td aj
      | td >= aj        = (ty, td - aj)
      | isLeap (ty - 1) = if td + 366 >= aj
                          then (ty - 1, td + 366 - aj)
                          else adjust (ty - 1) (td + 366) aj
      | otherwise       = if td + 365 >= aj
                          then (ty - 1, td + 365 - aj)
                          else adjust (ty - 1) (td + 365) aj
    isLeap year = year `rem` 4 == 0
              && (year `rem` 400 == 0 ||
                  year `rem` 100 /= 0)
    (months, daysArr) = if isLeap yy
      then (leapMonth, leapDayInMonth)
      else (normalMonth, normalDayInMonth)
    findMonth n = inlinePerformIO $ (,) <$> (peekElemOff months n) <*> (peekElemOff daysArr n)

----------------------------------------------------------------

normalMonthDays :: [Int]
normalMonthDays = [31,28,31,30,31,30,31,31,30,31,30,31]

leapMonthDays :: [Int]
leapMonthDays   = [31,29,31,30,31,30,31,31,30,31,30,31]

mkPtrInt :: [Int] -> Ptr Int
mkPtrInt = unsafePerformIO . newArray . concat . zipWith (flip replicate) [1..]

mkPtrInt2 :: [Int] -> Ptr Int
mkPtrInt2 = unsafePerformIO . newArray . concatMap (enumFromTo 1)

normalMonth :: Ptr Int
normalMonth = mkPtrInt normalMonthDays

normalDayInMonth :: Ptr Int
normalDayInMonth = mkPtrInt2 normalMonthDays

leapMonth :: Ptr Int
leapMonth = mkPtrInt leapMonthDays

leapDayInMonth :: Ptr Int
leapDayInMonth = mkPtrInt2 leapMonthDays

----------------------------------------------------------------

toHHMMSS :: Int -> (Int,Int,Int)
toHHMMSS x = (hh,mm,ss)
  where
    (hhmm,ss) = x `quotRem` 60
    (hh,mm) = hhmm `quotRem` 60
