{-# LINE 1 "Data/IP/Range.hs" #-}
# 1 "Data/IP/Range.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1











































































































# 9 "<command-line>" 2
# 1 "Data/IP/Range.hs"
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Data.IP.Range where

import Control.Monad
import Data.Bits
import Data.IP.Addr
import Data.IP.Mask
import Data.String
import Text.Appar.String

import GHC.Generics


----------------------------------------------------------------

{-|
  A unified data for 'AddrRange' 'IPv4' and 'AddrRange' 'IPv6'.
  To create this, use 'read' @\"192.0.2.0/24\"@ :: 'IPRange'.
  Also, @\"192.0.2.0/24\"@ can be used as literal with OverloadedStrings.

>>> (read "192.0.2.1/24" :: IPRange) == IPv4Range (read "192.0.2.0/24" :: AddrRange IPv4)
True
>>> (read "2001:db8:00:00:00:00:00:01/48" :: IPRange) == IPv6Range (read "2001:db8:00:00:00:00:00:01/48" :: AddrRange IPv6)
True
-}

data IPRange = IPv4Range { ipv4range :: AddrRange IPv4 }
             | IPv6Range { ipv6range :: AddrRange IPv6 }

        deriving (Eq, Generic)




----------------------------------------------------------------
--
-- Range
--

{-|
  The Addr range consists of an address, a contiguous mask,
  and mask length. The contiguous mask and the mask length
  are essentially same information but contained for pre
  calculation.

  To create this, use 'makeAddrRange' or 'read' @\"192.0.2.0/24\"@ :: 'AddrRange' 'IPv4'.
  Also, @\"192.0.2.0/24\"@ can be used as literal with OverloadedStrings.

>>> read "192.0.2.1/24" :: AddrRange IPv4
192.0.2.0/24
>>> read "2001:db8:00:00:00:00:00:01/48" :: AddrRange IPv6
2001:db8::/48
-}
data AddrRange a = AddrRange {
        -- |The 'addr' function returns an address from 'AddrRange'.
        addr :: !a
        -- |The 'mask' function returns a contiguous 'IP' mask from 'AddrRange'.
      , mask :: !a
        -- |The 'mlen' function returns a mask length from 'AddrRange'.
      , mlen :: {-# UNPACK #-} !Int
    }

    deriving (Eq, Ord, Generic)




----------------------------------------------------------------
--
-- Show
--

instance Show a => Show (AddrRange a) where
    show x = show (addr x) ++ "/" ++ show (mlen x)

instance Show IPRange where
    show (IPv4Range ip) = show ip
    show (IPv6Range ip) = show ip

----------------------------------------------------------------
--
-- Read
--

instance Read IPRange where
    readsPrec _ = parseIPRange

parseIPRange :: String -> [(IPRange,String)]
parseIPRange cs = case runParser ip4range cs of
    (Just ip,rest) -> [(IPv4Range ip,rest)]
    (Nothing,_)    -> case runParser ip6range cs of
        (Just ip,rest) -> [(IPv6Range ip,rest)]
        (Nothing,_) -> []

instance Read (AddrRange IPv4) where
    readsPrec _ = parseIPv4Range

instance Read (AddrRange IPv6) where
    readsPrec _ = parseIPv6Range

parseIPv4Range :: String -> [(AddrRange IPv4,String)]
parseIPv4Range cs = case runParser ip4range cs of
    (Nothing,_)    -> []
    (Just a4,rest) -> [(a4,rest)]

parseIPv6Range :: String -> [(AddrRange IPv6,String)]
parseIPv6Range cs = case runParser ip6range cs of
    (Nothing,_)    -> []
    (Just a6,rest) -> [(a6,rest)]

ip4range :: Parser (AddrRange IPv4)
ip4range = do
    ip <- ip4
    len <- option 32 $ char '/' >> dig
    check len
    let msk = maskIPv4 len
        adr = ip `maskedIPv4` msk
    return $ AddrRange adr msk len
  where
    check len = when (len < 0 || 32 < len) (fail "IPv4 mask length")

maskedIPv4 :: IPv4 -> IPv4 -> IPv4
IP4 a `maskedIPv4` IP4 m = IP4 (a .&. m)

ip6range :: Parser (AddrRange IPv6)
ip6range = do
    ip <- ip6
    len <- option 128 $ char '/' >> dig
    check len
    let msk = maskIPv6 len
        adr = ip `maskedIPv6` msk
    return $ AddrRange adr msk len
  where
    check len = when (len < 0 || 128 < len) (fail ("IPv6 mask length: " ++ show len))

maskedIPv6 :: IPv6 -> IPv6 -> IPv6
IP6 (a1,a2,a3,a4) `maskedIPv6` IP6 (m1,m2,m3,m4) = IP6 (a1.&.m1,a2.&.m2,a3.&.m3,a4.&.m4)

----------------------------------------------------------------
--
-- IsString
--

instance IsString IPRange where
    fromString = read

instance IsString (AddrRange IPv4) where
    fromString = read

instance IsString (AddrRange IPv6) where
    fromString = read
