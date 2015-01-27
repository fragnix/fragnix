{-# LINE 1 "Data/IP/Addr.hs" #-}

















































{-# LANGUAGE CPP #-}
module Data.IP.Addr where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List (foldl', intersperse)
import Data.String
import Data.Word
import Network.Socket
import Numeric (showHex, showInt)
import System.ByteOrder
import Text.Appar.String
import GHC.Generics

----------------------------------------------------------------

{-|
  A unified IP data for 'IPv4' and 'IPv6'.
  To create this, use the data constructors. Or use 'read' @\"192.0.2.1\"@ :: 'IP', for example. Also, @\"192.0.2.1\"@ can be used as literal with OverloadedStrings.

>>> (read "192.0.2.1" :: IP) == IPv4 (read "192.0.2.1" :: IPv4)
True
>>> (read "2001:db8:00:00:00:00:00:01" :: IP) == IPv6 (read "2001:db8:00:00:00:00:00:01" :: IPv6)
True
-}

data IP = IPv4 { ipv4 :: IPv4 }
        | IPv6 { ipv6 :: IPv6 }
        deriving (Eq, Ord, Generic)

instance Show IP where
    show (IPv4 ip) = show ip
    show (IPv6 ip) = show ip

----------------------------------------------------------------

-- This is host byte order
type IPv4Addr = Word32
type IPv6Addr = (Word32,Word32,Word32,Word32)

{-|
  The abstract data type to express an IPv4 address.
  To create this, use 'toIPv4'. Or use 'read' @\"192.0.2.1\"@ :: 'IPv4', for example. Also, @\"192.0.2.1\"@ can be used as literal with OverloadedStrings.

>>> read "192.0.2.1" :: IPv4
192.0.2.1
-}
newtype IPv4 = IP4 IPv4Addr
  deriving (Eq, Ord,Generic)

{-|
  The abstract data type to express an IPv6 address.
  To create this, use 'toIPv6'. Or use 'read' @\"2001:DB8::1\"@ :: 'IPv6', for example. Also, @\"2001:DB8::1\"@ can be used as literal with OverloadedStrings.

>>> read "2001:db8:00:00:00:00:00:01" :: IPv6
2001:db8::1
>>> read "2001:db8:11e:c00::101" :: IPv6
2001:db8:11e:c00::101
>>> read "2001:db8:11e:c00:aa:bb:192.0.2.1" :: IPv6
2001:db8:11e:c00:aa:bb:c000:201
>>> read "2001:db8::192.0.2.1" :: IPv6
2001:db8::c000:201
>>> read "0::ffff:192.0.2.1" :: IPv6
::ffff:192.0.2.1
>>> read "0::0:c000:201" :: IPv6
::192.0.2.1
>>> read "::0.0.0.1" :: IPv6
::1
-}
newtype IPv6 = IP6 IPv6Addr
  deriving (Eq, Ord,Generic)

----------------------------------------------------------------
--
-- Show
--

instance Show IPv4 where
    show ip = showIPv4 ip ""

instance Show IPv6 where
    show ip = showIPv6 ip ""

-- | Show an IPv4 address in the dot-decimal notation.
showIPv4 :: IPv4 -> ShowS
showIPv4 = foldr1 (.) . intersperse (showChar '.') . map showInt . fromIPv4

-- | Show an IPv6 address in the most appropriate notation, based on recommended
-- representation proposed by <http://tools.ietf.org/html/rfc5952 RFC 5952>.
--
-- /The implementation is completely compatible with the current implementation
-- of the `inet_ntop` function in glibc./
showIPv6 :: IPv6 -> ShowS
showIPv6 ip@(IP6 (a1,a2,a3,a4))
  -- IPv4-Mapped IPv6 Address
  | a1 == 0 && a2 == 0 && a3 == 0xffff =
      showString "::ffff:" . showIPv4 (IP4 a4)
  -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
  | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
      showString "::" . showIPv4 (IP4 a4)
  -- length of longest run > 1, replace it with "::"
  | end - begin > 1 =
      showFields prefix . showString "::" . showFields suffix
  -- length of longest run <= 1, don't use "::"
  | otherwise =
      showFields fields
  where
    fields = fromIPv6 ip
    showFields = foldr (.) id . intersperse (showChar ':') . map showHex
    prefix = take begin fields  -- fields before "::"
    suffix = drop end fields    -- fields after "::"
    begin = end + diff          -- the longest run of zeros
    (diff, end) = minimum $
        scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]

----------------------------------------------------------------
--
-- IntToIP
--

{-|
  The 'toIPv4' function takes a list of 'Int' and returns 'IPv4'.

>>> toIPv4 [192,0,2,1]
192.0.2.1
-}
toIPv4 :: [Int] -> IPv4
toIPv4 = IP4 . toWord32
  where
    toWord32 [a1,a2,a3,a4] = fromIntegral $ shift a1 24 + shift a2 16 + shift a3 8 + a4
    toWord32 _             = error "toWord32"

{-|
  The 'toIPv6' function takes a list of 'Int' and returns 'IPv6'.

>>> toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]
2001:db8::1
-}
toIPv6 :: [Int] -> IPv6
toIPv6 ad = IP6 (x1,x2,x3,x4)
  where
    [x1,x2,x3,x4] = map toWord32 $ split2 ad
    split2 [] = []
    split2 x  = take 2 x : split2 (drop 2 x)
    toWord32 [a1,a2] = fromIntegral $ shift a1 16 + a2
    toWord32 _       = error "toWord32"

----------------------------------------------------------------
--
-- IPToInt
--

{-|
  The 'fromIPv4' function converts 'IPv4' to a list of 'Int'.

>>> fromIPv4 (toIPv4 [192,0,2,1])
[192,0,2,1]
-}
fromIPv4 :: IPv4 -> [Int]
fromIPv4 (IP4 w) = map (\n -> fromEnum $ (w `shiftR` n) .&. 0xff) [0o30, 0o20, 0o10, 0o00]

{-|
  The 'toIPv6' function converts 'IPv6' to a list of 'Int'.

>>> fromIPv6 (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1])
[8193,3512,0,0,0,0,0,1]
-}
fromIPv6 :: IPv6 -> [Int]
fromIPv6 (IP6 (w1, w2, w3, w4)) = map fromEnum (concatMap split [w1,w2,w3,w4])
  where
    split :: Word32 -> [Word32]
    split n = [n `shiftR` 0x10 .&. 0xffff, n .&. 0xffff]

----------------------------------------------------------------
--
-- Read
--

instance Read IP where
    readsPrec _ = parseIP

instance Read IPv4 where
    readsPrec _ = parseIPv4

instance Read IPv6 where
    readsPrec _ = parseIPv6

parseIP :: String -> [(IP,String)]
parseIP cs = case runParser ip4 cs of
    (Just ip,rest) -> [(IPv4 ip,rest)]
    (Nothing,_) -> case runParser ip6 cs of
        (Just ip,rest) -> [(IPv6 ip,rest)]
        (Nothing,_) -> []

parseIPv4 :: String -> [(IPv4,String)]
parseIPv4 cs = case runParser ip4 cs of
    (Nothing,_)    -> []
    (Just a4,rest) -> [(a4,rest)]

parseIPv6 :: String -> [(IPv6,String)]
parseIPv6 cs = case runParser ip6 cs of
    (Nothing,_)    -> []
    (Just a6,rest) -> [(a6,rest)]

----------------------------------------------------------------
--
-- IsString
--

instance IsString IP where
    fromString = read

instance IsString IPv4 where
    fromString = read

instance IsString IPv6 where
    fromString = read

----------------------------------------------------------------
--
-- IPv4 Parser
--

dig :: Parser Int
dig = 0 <$ char '0'
  <|> toInt <$> oneOf ['1'..'9'] <*> many digit
  where
    toInt n ns = foldl' (\x y -> x * 10 + y) 0 . map digitToInt $ n : ns

ip4 :: Parser IPv4
ip4 = skipSpaces >> toIPv4 <$> ip4'

ip4' :: Parser [Int]
ip4' = do
    as <- dig `sepBy1` char '.'
    check as
    return as
  where
    test errmsg adr = when (adr < 0 || 255 < adr) (fail errmsg)
    check as = do
        let errmsg = "IPv4 adddress"
        when (length as /= 4) (fail errmsg)
        mapM_ (test errmsg) as

skipSpaces :: Parser ()
skipSpaces = void $ many (char ' ')

----------------------------------------------------------------
--
-- IPv6 Parser (RFC 4291)
--

hex :: Parser Int
hex = do
    ns <- some hexDigit
    check ns
    let ms = map digitToInt ns
        val = foldl' (\x y -> x * 16 + y) 0 ms
    return val
  where
    check ns = when (length ns > 4) (fail "IPv6 address -- more than 4 hex")

colon2 :: Parser ()
colon2 = void $ string "::"

format :: [Int] -> [Int] -> Parser [Int]
format bs1 bs2 = do
    let len1 = length bs1
        len2 = length bs2
    when (len1 > 7) (fail "IPv6 address1")
    when (len2 > 7) (fail "IPv6 address2")
    let len = 8 - len1 - len2
    when (len <= 0) (fail "IPv6 address3")
    let spring = replicate len 0
    return $ bs1 ++ spring ++ bs2

ip6 :: Parser IPv6
ip6 = skipSpaces >> toIPv6 <$> ip6'

ip6' :: Parser [Int]
ip6' = ip4Embedded
   <|> do colon2
          bs <- option [] hexcolon
          format [] bs
   <|> try (do rs <- hexcolon
               check rs
               return rs)
   <|> do bs1 <- hexcolon2
          bs2 <- option [] hexcolon
          format bs1 bs2
  where
    hexcolon = hex `sepBy1` char ':'
    hexcolon2 = manyTill (hex <* char ':') (char ':')
    check bs = when (length bs /= 8) (fail "IPv6 address4")

ip4Embedded :: Parser [Int]
ip4Embedded = try (do colon2
                      bs <- beforeEmbedded
                      embedded <- ip4'
                      format [] (bs ++ ip4ToIp6 embedded))
              -- matches 2001:db8::192.0.2.1
          <|> try (do bs1 <- manyTill (try $ hex <* char ':') (char ':')
                      bs2 <- option [] beforeEmbedded
                      embedded <- ip4'
                      format bs1 (bs2 ++ ip4ToIp6 embedded))
              -- matches 2001:db8:11e:c00:aa:bb:192.0.2.1
          <|> try (do bs <- beforeEmbedded
                      embedded <- ip4'
                      let rs = bs ++ ip4ToIp6 embedded
                      check rs
                      return rs)
  where
    beforeEmbedded = many $ try $ hex <* char ':'
    ip4ToIp6 [a,b,c,d] = [ a `shiftL` 8 .|. b
                         , c `shiftL` 8 .|. d ]
    ip4ToIp6 _ = error "ip4ToIp6"
    check bs = when (length bs /= 8) (fail "IPv6 address4")

----------------------------------------------------------------
--
-- HostAddress and HostAddress6
--

-- | The 'fromHostAddress' function converts 'HostAddress' to 'IPv4'.
fromHostAddress :: HostAddress -> IPv4
fromHostAddress addr4
  | byteOrder == LittleEndian = IP4 $ fixByteOrder addr4
  | otherwise                 = IP4 addr4

-- | The 'toHostAddress' function converts 'IPv4' to 'HostAddress'.
toHostAddress :: IPv4 -> HostAddress
toHostAddress (IP4 addr4)
  | byteOrder == LittleEndian = fixByteOrder addr4
  | otherwise                 = addr4

-- | The 'fromHostAddress6' function converts 'HostAddress6' to 'IPv6'.
fromHostAddress6 :: HostAddress6 -> IPv6
fromHostAddress6 = IP6

-- | The 'toHostAddress6' function converts 'IPv6' to 'HostAddress6'.
toHostAddress6 :: IPv6 -> HostAddress6
toHostAddress6 (IP6 addr6) = addr6

fixByteOrder :: Word32 -> Word32
fixByteOrder s = d1 .|. d2 .|. d3 .|. d4
  where
    d1 = shiftL s 24
    d2 = shiftL s  8 .&. 0x00ff0000
    d3 = shiftR s  8 .&. 0x0000ff00
    d4 = shiftR s 24 .&. 0x000000ff
