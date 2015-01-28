{-# LANGUAGE Haskell2010, DeriveGeneric #-}
{-# LINE 1 "Data/IP/Mask.hs" #-}
module Data.IP.Mask where

import Data.Bits
import Data.IP.Addr
import Data.Word
import Data.IntMap hiding (map)

----------------------------------------------------------------

maskIPv4 :: Int -> IPv4
maskIPv4 len = IP4 (masksIPv4 ! len)

maskIPv6 :: Int -> IPv6
maskIPv6 len = IP6 (masksIPv6 ! len)

masksWord32 :: [Word32]
masksWord32 = take 33 $ iterate (`shift` 1) 0xffffffff

masksIPv4 :: IntMap IPv4Addr
masksIPv4 = fromList $ zip [32,31..0] masksWord32

masksIPv6 :: IntMap IPv6Addr
masksIPv6 = fromList $ zip [128,127..0] ms
  where
    ms = m0 ++ m1 ++ m2 ++ m3 ++ m4
    m0 = [(all1,all1,all1,all1)]
    m1 = map (\vmsk -> (all1,all1,all1,vmsk)) masks
    m2 = map (\vmsk -> (all1,all1,vmsk,all0)) masks
    m3 = map (\vmsk -> (all1,vmsk,all0,all0)) masks
    m4 = map (\vmsk -> (vmsk,all0,all0,all0)) masks
    masks = tail masksWord32
    all1 = 0xffffffff
    all0 = 0x00000000
