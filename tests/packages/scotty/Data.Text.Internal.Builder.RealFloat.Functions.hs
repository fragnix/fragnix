{-# LINE 1 "Data/Text/Internal/Builder/RealFloat/Functions.hs" #-}
# 1 "Data/Text/Internal/Builder/RealFloat/Functions.hs"
# 1 "<command-line>"
# 10 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 10 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1


















































































































# 10 "<command-line>" 2
# 1 "Data/Text/Internal/Builder/RealFloat/Functions.hs"
{-# LANGUAGE CPP #-}

-- |
-- Module:    Data.Text.Internal.Builder.RealFloat.Functions
-- Copyright: (c) The University of Glasgow 1994-2002
-- License:   see libraries/base/LICENSE
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!

module Data.Text.Internal.Builder.RealFloat.Functions
    (
      roundTo
    ) where

roundTo :: Int -> [Int] -> (Int,[Int])



roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i
  base = 10

