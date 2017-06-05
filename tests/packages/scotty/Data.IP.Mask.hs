{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/IP/Mask.hs" #-}
module Data.IP.Mask where

import Data.Bits
import Data.IP.Addr
import Data.Word

maskIPv4 :: Int -> IPv4
maskIPv4 len =
    IP4 $ complement $ 0xffffffff `shift` (-len)

maskIPv6 :: Int -> IPv6
maskIPv6 len =
    IP6 $ toIP6Addr $ bimapTup complement $
            (0xffffffffffffffff, 0xffffffffffffffff) `shift128` (-len)
 where
   bimapTup f (x,y) = (f x, f y)

shift128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shift128 x i
    | i < 0  = x `shiftR128` (-i)
    | i > 0  = x `shiftL128` i
    | otherwise = x

shiftL128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftL128 (h, l) i =
        ( (h `shiftL` i) .|. (l `shift` (i - 64) ), (l `shiftL` i))

shiftR128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftR128 (h, l) i =
    (h `shiftR` i, (l `shiftR` i) .|. h `shift` (64 - i) )

fromIP6Addr :: IPv6Addr -> (Word64, Word64)
fromIP6Addr (w3, w2, w1, w0) =
   ( (fromIntegral w3 `shiftL` 32) .|. fromIntegral w2
   , (fromIntegral w1 `shiftL` 32) .|. fromIntegral w0
   )

toIP6Addr :: (Word64, Word64) -> IPv6Addr
toIP6Addr (h, l) =
    ( fromIntegral $ (h `shiftR` 32) .&. m
    , fromIntegral $ h .&. m
    , fromIntegral $ (l `shiftR` 32) .&. m
    , fromIntegral $ l .&. m
    )
  where m = 0xffffffff
