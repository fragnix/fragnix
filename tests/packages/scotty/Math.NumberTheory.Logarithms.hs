{-# LINE 1 "src/Math/NumberTheory/Logarithms.hs" #-}























































{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

-- | Integer logarithm, copied from Daniel Fischer's @arithmoi@
module Math.NumberTheory.Logarithms ( integerLog10' ) where

import GHC.Base ( Int(I#), Word#, Int#
                , int2Word#, eqWord#, neWord#, (-#), and#, uncheckedShiftRL#

                , isTrue#
                )

import GHC.Integer.Logarithms (integerLog2#, wordLog2#)

-- | Only defined for positive inputs!
integerLog10' :: Integer -> Int
integerLog10' n
  | n < 10      = 0
  | n < 100     = 1
  | otherwise   = ex + integerLog10' (n `quot` integerPower 10 ex)
    where
      ln = I# (integerLog2# n)
      -- u/v is a good approximation of log 2/log 10
      u  = 1936274
      v  = 6432163
      -- so ex is a good approximation to integerLogBase 10 n
      ex = fromInteger ((u * fromIntegral ln) `quot` v)

-- | Power of an 'Integer' by the left-to-right repeated squaring algorithm.
--   This needs two multiplications in each step while the right-to-left
--   algorithm needs only one multiplication for 0-bits, but here the
--   two factors always have approximately the same size, which on average
--   gains a bit when the result is large.
--
--   For small results, it is unlikely to be any faster than '(^)', quite
--   possibly slower (though the difference shouldn't be large), and for
--   exponents with few bits set, the same holds. But for exponents with
--   many bits set, the speedup can be significant.
--
--   /Warning:/ No check for the negativity of the exponent is performed,
--   a negative exponent is interpreted as a large positive exponent.
integerPower :: Integer -> Int -> Integer
integerPower b (I# e#) = power b (int2Word# e#)

power :: Integer -> Word# -> Integer
power b w#
  | isTrue# (w# `eqWord#` 0##) = 1
  | isTrue# (w# `eqWord#` 1##) = b
  | otherwise           = go (wordLog2# w# -# 1#) b (b*b)
    where
      go 0# l h = if isTrue# ((w# `and#` 1##) `eqWord#` 0##) then l*l else (l*h)
      go i# l h
        | w# `hasBit#` i#   = go (i# -# 1#) (l*h) (h*h)
        | otherwise         = go (i# -# 1#) (l*l) (l*h)

-- | A raw version of testBit for 'Word#'.
hasBit# :: Word# -> Int# -> Bool
hasBit# w# i# = isTrue# (((w# `uncheckedShiftRL#` i#) `and#` 1##) `neWord#` 0##)

