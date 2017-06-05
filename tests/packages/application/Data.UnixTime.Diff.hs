{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/UnixTime/Diff.hs" #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UnixTime.Diff (
    diffUnixTime
  , addUnixDiffTime
  , secondsToUnixDiffTime
  , microSecondsToUnixDiffTime
  ) where

import Data.UnixTime.Types
import Data.Int
import Foreign.C.Types

----------------------------------------------------------------

calc :: CTime -> Int32 -> UnixDiffTime
calc sec usec = uncurry UnixDiffTime . adjust sec $ usec

calc' :: CTime -> Int32 -> UnixDiffTime
calc' sec usec = uncurry UnixDiffTime . slowAdjust sec $ usec

calcU :: CTime -> Int32 -> UnixTime
calcU sec usec = uncurry UnixTime . adjust sec $ usec

-- | Arithmetic operations where (1::UnixDiffTime) means 1 second.
--
-- >>> (3 :: UnixDiffTime) + 2
-- UnixDiffTime {udtSeconds = 5, udtMicroSeconds = 0}
-- >>> (2 :: UnixDiffTime) - 5
-- UnixDiffTime {udtSeconds = -3, udtMicroSeconds = 0}
-- >>> (3 :: UnixDiffTime) * 2
-- UnixDiffTime {udtSeconds = 6, udtMicroSeconds = 0}

instance Num UnixDiffTime where
    UnixDiffTime s1 u1 + UnixDiffTime s2 u2 = calc (s1+s2) (u1+u2)
    UnixDiffTime s1 u1 - UnixDiffTime s2 u2 = calc (s1-s2) (u1-u2)
    UnixDiffTime s1 u1 * UnixDiffTime s2 u2 = calc' (s1*s2) (u1*u2)
    negate (UnixDiffTime s u) = UnixDiffTime (-s) (-u)
    abs (UnixDiffTime s u) = UnixDiffTime (abs s) (abs u)
    signum (UnixDiffTime s u)
         | s == 0 && u == 0 = 0
         | s > 0            = 1
         | otherwise        = -1
    fromInteger i = UnixDiffTime (fromInteger i) 0

{-# RULES "Integral->UnixDiffTime" fromIntegral = secondsToUnixDiffTime #-}

instance Real UnixDiffTime where
        toRational = toFractional

{-# RULES "UnixDiffTime->Fractional" realToFrac = toFractional #-}

----------------------------------------------------------------

-- | Calculating difference between two 'UnixTime'.
--
-- >>> UnixTime 100 2000 `diffUnixTime` UnixTime 98 2100
-- UnixDiffTime {udtSeconds = 1, udtMicroSeconds = 999900}
--

diffUnixTime :: UnixTime -> UnixTime -> UnixDiffTime
diffUnixTime (UnixTime s1 u1) (UnixTime s2 u2) = calc (s1-s2) (u1-u2)

-- | Adding difference to 'UnixTime'.
--
-- >>> UnixTime 100 2000 `addUnixDiffTime` microSecondsToUnixDiffTime (-1003000)
-- UnixTime {utSeconds = 98, utMicroSeconds = 999000}

addUnixDiffTime :: UnixTime -> UnixDiffTime -> UnixTime
addUnixDiffTime (UnixTime s1 u1) (UnixDiffTime s2 u2) = calcU (s1+s2) (u1+u2)

-- | Creating difference from seconds.
--
-- >>> secondsToUnixDiffTime 100
-- UnixDiffTime {udtSeconds = 100, udtMicroSeconds = 0}

secondsToUnixDiffTime :: (Integral a) => a -> UnixDiffTime
secondsToUnixDiffTime sec = UnixDiffTime (fromIntegral sec) 0
{-# INLINE secondsToUnixDiffTime #-}

-- | Creating difference from micro seconds.
--
-- >>> microSecondsToUnixDiffTime 12345678
-- UnixDiffTime {udtSeconds = 12, udtMicroSeconds = 345678}
--
-- >>> microSecondsToUnixDiffTime (-12345678)
-- UnixDiffTime {udtSeconds = -12, udtMicroSeconds = -345678}

microSecondsToUnixDiffTime :: (Integral a) => a -> UnixDiffTime
microSecondsToUnixDiffTime usec = calc (fromIntegral s) (fromIntegral u)
  where
    (s,u) = secondMicro usec
{-# INLINE microSecondsToUnixDiffTime #-}

----------------------------------------------------------------

adjust :: CTime -> Int32 -> (CTime, Int32)
adjust sec usec
  | sec >= 0  = ajp
  | otherwise = ajm
  where
    micro  = 1000000
    mmicro = - micro
    ajp
     | usec >= micro  = (sec + 1, usec - micro)
     | usec >= 0      = (sec, usec)
     | otherwise      = (sec - 1, usec + micro)
    ajm
     | usec <= mmicro = (sec - 1, usec + micro)
     | usec <= 0      = (sec, usec)
     | otherwise      = (sec + 1, usec - micro)

slowAdjust :: CTime -> Int32 -> (CTime, Int32)
slowAdjust sec usec = (sec + fromIntegral s, usec - u)
  where
    (s,u) = secondMicro usec

secondMicro :: Integral a => a -> (a,a)
secondMicro usec = usec `quotRem` 1000000

toFractional :: Fractional a => UnixDiffTime -> a
toFractional (UnixDiffTime s u) = realToFrac s + realToFrac u / 1000000
{-# SPECIALIZE toFractional :: UnixDiffTime -> Double #-}
