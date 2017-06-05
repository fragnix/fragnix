{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Network/HTTP/Date/Formatter.hs" #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Date.Formatter (formatHTTPDate) where

import Data.ByteString.Char8 ()
import Data.ByteString.Internal
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Network.HTTP.Date.Types

----------------------------------------------------------------

-- | Generating HTTP Date in RFC1123 style.
--
-- >>> formatHTTPDate defaultHTTPDate {hdYear = 1994, hdMonth = 11, hdDay = 15, hdHour = 8, hdMinute = 12, hdSecond = 31, hdWkday = 2}
-- "Tue, 15 Nov 1994 08:12:31 GMT"

formatHTTPDate :: HTTPDate -> ByteString
formatHTTPDate hd =
    unsafeCreate 29 $ \ptr -> do
        cpy3 ptr weekDays (3 * w)
        poke (ptr `plusPtr`  3) comma
        poke (ptr `plusPtr`  4) spc
        int2 (ptr `plusPtr`  5) d
        poke (ptr `plusPtr`  7) spc
        cpy3 (ptr `plusPtr`  8) months (3 * m)
        poke (ptr `plusPtr` 11) spc
        int4 (ptr `plusPtr` 12) y
        poke (ptr `plusPtr` 16) spc
        int2 (ptr `plusPtr` 17) h
        poke (ptr `plusPtr` 19) colon
        int2 (ptr `plusPtr` 20) n
        poke (ptr `plusPtr` 22) colon
        int2 (ptr `plusPtr` 23) s
        poke (ptr `plusPtr` 25) spc
        poke (ptr `plusPtr` 26) (71 :: Word8)
        poke (ptr `plusPtr` 27) (77 :: Word8)
        poke (ptr `plusPtr` 28) (84 :: Word8)
  where
    y = hdYear hd
    m = hdMonth hd
    d = hdDay hd
    h = hdHour hd
    n = hdMinute hd
    s = hdSecond hd
    w = hdWkday hd
    cpy3 :: Ptr Word8 -> ForeignPtr Word8 -> Int -> IO ()
    cpy3 ptr p o = withForeignPtr p $ \fp ->
      memcpy ptr (fp `plusPtr` o) 3

----------------------------------------------------------------

int2 :: Ptr Word8 -> Int -> IO ()
int2 ptr n
  | n < 10 = do
      poke ptr zero
      poke (ptr `plusPtr` 1) (i2w8 n)
  | otherwise = do
      poke ptr               (i2w8 (n `quot` 10))
      poke (ptr `plusPtr` 1) (i2w8 (n `rem` 10))

int4 :: Ptr Word8 -> Int -> IO ()
int4 ptr n0 = do
    let (n1,x1) = n0 `quotRem` 10
        (n2,x2) = n1 `quotRem` 10
        (x4,x3) = n2 `quotRem` 10
    poke ptr               (i2w8 x4)
    poke (ptr `plusPtr` 1) (i2w8 x3)
    poke (ptr `plusPtr` 2) (i2w8 x2)
    poke (ptr `plusPtr` 3) (i2w8 x1)

i2w8 :: Int -> Word8
i2w8 n = fromIntegral n + zero

----------------------------------------------------------------

months :: ForeignPtr Word8
months = let (PS p _ _) = "___JanFebMarAprMayJunJulAugSepOctNovDec" in p

weekDays :: ForeignPtr Word8
weekDays = let (PS p _ _) = "___MonTueWedThuFriSatSun" in p

----------------------------------------------------------------

spc :: Word8
spc = 32

comma :: Word8
comma = 44

colon :: Word8
colon = 58

zero :: Word8
zero = 48
