{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/IP/Op.hs" #-}
module Data.IP.Op where

import Data.Bits
import Data.IP.Addr
import Data.IP.Mask
import Data.IP.Range

----------------------------------------------------------------

{-|

>>> toIPv4 [127,0,2,1] `masked` intToMask 7
126.0.0.0
-}
class Eq a => Addr a where
    {-|
      The 'masked' function takes an 'Addr' and a contiguous
      mask and returned a masked 'Addr'.
    -}
    masked :: a -> a -> a
    {-|

      The 'intToMask' function takes an 'Int' representing the number of bits to
      be set in the returned contiguous mask. When this integer is positive the
      bits will be starting from the MSB and from the LSB otherwise.

      >>> intToMask 16 :: IPv4
      255.255.0.0

      >>> intToMask (-16) :: IPv4
      0.0.255.255

      >>> intToMask 16 :: IPv6
      ffff::

      >>> intToMask (-16) :: IPv6
      ::ffff

    -}
    intToMask :: Int -> a

instance Addr IPv4 where
    masked    = maskedIPv4
    intToMask = maskIPv4

instance Addr IPv6 where
    masked    = maskedIPv6
    intToMask = maskIPv6

----------------------------------------------------------------

{-|
  The >:> operator takes two 'AddrRange'. It returns 'True' if
  the first 'AddrRange' contains the second 'AddrRange'. Otherwise,
  it returns 'False'.

>>> makeAddrRange ("127.0.2.1" :: IPv4) 8 >:> makeAddrRange "127.0.2.1" 24
True
>>> makeAddrRange ("127.0.2.1" :: IPv4) 24 >:> makeAddrRange "127.0.2.1" 8
False
>>> makeAddrRange ("2001:DB8::1" :: IPv6) 16 >:> makeAddrRange "2001:DB8::1" 32
True
>>> makeAddrRange ("2001:DB8::1" :: IPv6) 32 >:> makeAddrRange "2001:DB8::1" 16
False
-}
(>:>) :: Addr a => AddrRange a -> AddrRange a -> Bool
a >:> b = mlen a <= mlen b && (addr b `masked` mask a) == addr a

{-|
  The 'toMatchedTo' function take an 'Addr' address and an 'AddrRange',
  and returns 'True' if the range contains the address.

>>> ("127.0.2.0" :: IPv4) `isMatchedTo` makeAddrRange "127.0.2.1" 24
True
>>> ("127.0.2.0" :: IPv4) `isMatchedTo` makeAddrRange "127.0.2.1" 32
False
>>> ("2001:DB8::1" :: IPv6) `isMatchedTo` makeAddrRange "2001:DB8::1" 32
True
>>> ("2001:DB8::" :: IPv6) `isMatchedTo` makeAddrRange "2001:DB8::1" 128
False
-}

isMatchedTo :: Addr a => a -> AddrRange a -> Bool
isMatchedTo a r = a `masked` mask r == addr r

{-|
  The 'makeAddrRange' functions takes an 'Addr' address and a mask
  length. It creates a bit mask from the mask length and masks
  the 'Addr' address, then returns 'AddrRange' made of them.

>>> makeAddrRange (toIPv4 [127,0,2,1]) 8
127.0.0.0/8
>>> makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 8
2000::/8
-}
makeAddrRange :: Addr a => a -> Int -> AddrRange a
makeAddrRange ad len = AddrRange adr msk len
  where
    msk = intToMask len
    adr = ad `masked` msk


-- | Convert IPv4 range to IPV4-embedded-in-IPV6 range
ipv4RangeToIPv6 :: AddrRange IPv4 -> AddrRange IPv6
ipv4RangeToIPv6 range =
  makeAddrRange (toIPv6 [0,0,0,0,0,0xffff, (i1 `shift` 8) .|. i2, (i3 `shift` 8) .|. i4]) (masklen + 96)
  where
    (ip, masklen) = addrRangePair range
    [i1,i2,i3,i4] = fromIPv4 ip


{-|
  The 'unmakeAddrRange' functions take a 'AddrRange' and
  returns the network address and a mask length.

>>> addrRangePair ("127.0.0.0/8" :: AddrRange IPv4)
(127.0.0.0,8)
>>> addrRangePair ("2000::/8" :: AddrRange IPv6)
(2000::,8)
-}
addrRangePair :: Addr a => AddrRange a -> (a, Int)
addrRangePair (AddrRange adr _ len) = (adr, len)
