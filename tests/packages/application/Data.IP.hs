{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/IP.hs" #-}
{-|
  Data structures to express IPv4, IPv6 and IP range.
-}
module Data.IP (
  -- * Documentation
  -- ** IP data
    IP (..)
  , IPv4, toIPv4, fromIPv4, fromHostAddress, toHostAddress
  , IPv6, toIPv6, toIPv6b, fromIPv6, fromIPv6b, fromHostAddress6, toHostAddress6
  -- ** IP range data
  , IPRange (..)
  , AddrRange (addr, mask, mlen)
  -- ** Address class
  , Addr (..)
  , makeAddrRange, (>:>), isMatchedTo, addrRangePair
  , ipv4RangeToIPv6, ipv4ToIPv6
  ) where

import Data.IP.Addr
import Data.IP.Op
import Data.IP.Range
