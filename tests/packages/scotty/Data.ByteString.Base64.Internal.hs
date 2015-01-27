{-# LINE 1 "Data/ByteString/Base64/Internal.hs" #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.ByteString.Base64.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base64-encoded strings.

module Data.ByteString.Base64.Internal
    (
      encodeWith
    , decodeWithTable
    , decodeLenientWithTable
    , mkEncodeTable
    , joinWith
    , done
    , peek8, poke8, peek8_32
    , reChunkIn
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, memcpy,
                                 unsafeCreate)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import System.IO.Unsafe (unsafePerformIO)

peek8 :: Ptr Word8 -> IO Word8
peek8 = peek

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = poke

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek8

-- | Encode a string into base64 form.  The result will always be a multiple
-- of 4 bytes in length.
encodeWith :: EncodeTable -> ByteString -> ByteString
encodeWith (ET alfaFP encodeTable) (PS sfp soff slen)
    | slen > maxBound `div` 4 =
        error "Data.ByteString.Base64.encode: input too long"
    | otherwise = unsafePerformIO $ do
  let dlen = ((slen + 2) `div` 3) * 4
  dfp <- mallocByteString dlen
  withForeignPtr alfaFP $ \aptr ->
    withForeignPtr encodeTable $ \ep ->
      withForeignPtr sfp $ \sptr -> do
        let aidx n = peek8 (aptr `plusPtr` n)
            sEnd = sptr `plusPtr` (slen + soff)
            fill !dp !sp
              | sp `plusPtr` 2 >= sEnd = complete (castPtr dp) sp
              | otherwise = {-# SCC "encode/fill" #-} do
              i <- peek8_32 sp
              j <- peek8_32 (sp `plusPtr` 1)
              k <- peek8_32 (sp `plusPtr` 2)
              let w = (i `shiftL` 16) .|. (j `shiftL` 8) .|. k
                  enc = peekElemOff ep . fromIntegral
              poke dp =<< enc (w `shiftR` 12)
              poke (dp `plusPtr` 2) =<< enc (w .&. 0xfff)
              fill (dp `plusPtr` 4) (sp `plusPtr` 3)
            complete dp sp
                | sp == sEnd = return ()
                | otherwise  = {-# SCC "encode/complete" #-} do
              let peekSP n f = (f . fromIntegral) `fmap` peek8 (sp `plusPtr` n)
                  twoMore    = sp `plusPtr` 2 == sEnd
                  equals     = 0x3d :: Word8
                  {-# INLINE equals #-}
              !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
              !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))
              !b' <- if twoMore
                     then peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
                     else return b
              poke8 dp =<< aidx a
              poke8 (dp `plusPtr` 1) =<< aidx b'
              !c <- if twoMore
                    then aidx =<< peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
                    else return equals
              poke8 (dp `plusPtr` 2) c
              poke8 (dp `plusPtr` 3) equals
        withForeignPtr dfp $ \dptr ->
          fill (castPtr dptr) (sptr `plusPtr` soff)
  return $! PS dfp 0 dlen

data EncodeTable = ET (ForeignPtr Word8) (ForeignPtr Word16)

-- The encoding table is constructed such that the expansion of a 12-bit
-- block to a 16-bit block can be done by a single Word16 copy from the
-- correspoding table entry to the target address. The 16-bit blocks are
-- stored in big-endian order, as the indices into the table are built in
-- big-endian order.
mkEncodeTable :: ByteString -> EncodeTable
mkEncodeTable alphabet@(PS afp _ _) =
    case table of PS fp _ _ -> ET afp (castForeignPtr fp)
  where
    ix    = fromIntegral . B.index alphabet
    table = B.pack $ concat $ [ [ix j, ix k] | j <- [0..63], k <- [0..63] ]

-- | Efficiently intersperse a terminator string into another at
-- regular intervals, and terminate the input with it.
--
-- Examples:
--
-- > joinWith "|" 2 "----" = "--|--|"
--
-- > joinWith "\r\n" 3 "foobarbaz" = "foo\r\nbar\r\nbaz\r\n"
-- > joinWith "x" 3 "fo" = "fox"
joinWith :: ByteString  -- ^ String to intersperse and end with
         -> Int         -- ^ Interval at which to intersperse, in bytes
         -> ByteString  -- ^ String to transform
         -> ByteString
joinWith brk@(PS bfp boff blen) every bs@(PS sfp soff slen)
    | every <= 0 = error "invalid interval"
    | blen <= 0  = bs
    | B.null bs = brk
    | otherwise =
  unsafeCreate dlen $ \dptr ->
    withForeignPtr bfp $ \bptr -> do
      withForeignPtr sfp $ \sptr -> do
          let bp = bptr `plusPtr` boff
              sp0 = sptr `plusPtr` soff
              sLast = sp0 `plusPtr` (every * numBreaks)
              loop !dp !sp
                  | sp == sLast = do
                      let n = sp0 `plusPtr` slen `minusPtr` sp
                      memcpy dp sp (fromIntegral n)
                      memcpy (dp `plusPtr` n) bp (fromIntegral blen)
                  | otherwise = do
                memcpy dp sp (fromIntegral every)
                let dp' = dp `plusPtr` every
                memcpy dp' bp (fromIntegral blen)
                loop (dp' `plusPtr` blen) (sp `plusPtr` every)
          loop dptr sp0
  where dlast = slen + blen * numBreaks
        dlen | slen `mod` every > 0 = dlast + blen
             | otherwise            = dlast
        numBreaks = slen `div` every

-- | Decode a base64-encoded string.  This function strictly follows
-- the specification in RFC 4648,
-- <http://www.apps.ietf.org/rfc/rfc4648.html>.
-- This function takes the decoding table (for @base64@ or @base64url@) as
-- the first paramert.
decodeWithTable :: ForeignPtr Word8 -> ByteString -> Either String ByteString
decodeWithTable decodeFP (PS sfp soff slen)
    | drem /= 0 = Left "invalid padding"
    | dlen <= 0 = Right B.empty
    | otherwise = unsafePerformIO $ do
  dfp <- mallocByteString dlen
  withForeignPtr decodeFP $ \ !decptr -> do
    let finish dbytes = return . Right $! if dbytes > 0
                                          then PS dfp 0 dbytes
                                          else B.empty
        bail = return . Left
    withForeignPtr sfp $ \ !sptr -> do
      let sEnd = sptr `plusPtr` (slen + soff)
          look p = do
            ix <- fromIntegral `fmap` peek8 p
            v <- peek8 (decptr `plusPtr` ix)
            return $! fromIntegral v :: IO Word32
          fill !dp !sp !n
            | sp >= sEnd = finish n
            | otherwise = {-# SCC "decodeWithTable/fill" #-} do
            a <- look sp
            b <- look (sp `plusPtr` 1)
            c <- look (sp `plusPtr` 2)
            d <- look (sp `plusPtr` 3)
            let w = (a `shiftL` 18) .|. (b `shiftL` 12) .|.
                    (c `shiftL` 6) .|. d
            if a == done || b == done
              then bail $ "invalid padding near offset " ++
                   show (sp `minusPtr` sptr)
              else if a .|. b .|. c .|. d == x
              then bail $ "invalid base64 encoding near offset " ++
                   show (sp `minusPtr` sptr)
              else do
                poke8 dp $ fromIntegral (w `shiftR` 16)
                if c == done
                  then finish $ n + 1
                  else do
                    poke8 (dp `plusPtr` 1) $ fromIntegral (w `shiftR` 8)
                    if d == done
                      then finish $! n + 2
                      else do
                        poke8 (dp `plusPtr` 2) $ fromIntegral w
                        fill (dp `plusPtr` 3) (sp `plusPtr` 4) (n+3)
      withForeignPtr dfp $ \dptr ->
        fill dptr (sptr `plusPtr` soff) 0
  where (di,drem) = slen `divMod` 4
        dlen = di * 3

-- | Decode a base64-encoded string.  This function is lenient in
-- following the specification from RFC 4648,
-- <http://www.apps.ietf.org/rfc/rfc4648.html>, and will not generate
-- parse errors no matter how poor its input.
-- This function takes the decoding table (for @base64@ or @base64url@) as
-- the first paramert.
decodeLenientWithTable :: ForeignPtr Word8 -> ByteString -> ByteString
decodeLenientWithTable decodeFP (PS sfp soff slen)
    | dlen <= 0 = B.empty
    | otherwise = unsafePerformIO $ do
  dfp <- mallocByteString dlen
  withForeignPtr decodeFP $ \ !decptr ->
    withForeignPtr sfp $ \ !sptr -> do
      let finish dbytes
              | dbytes > 0 = return (PS dfp 0 dbytes)
              | otherwise  = return B.empty
          sEnd = sptr `plusPtr` (slen + soff)
          fill !dp !sp !n
            | sp >= sEnd = finish n
            | otherwise = {-# SCC "decodeLenientWithTable/fill" #-}
            let look :: Bool -> Ptr Word8
                     -> (Ptr Word8 -> Word32 -> IO ByteString)
                     -> IO ByteString
                {-# INLINE look #-}
                look skipPad p0 f = go p0
                  where
                    go p | p >= sEnd = f (sEnd `plusPtr` (-1)) done
                         | otherwise = {-# SCC "decodeLenient/look" #-} do
                      ix <- fromIntegral `fmap` peek8 p
                      v <- peek8 (decptr `plusPtr` ix)
                      if v == x || (v == done && skipPad)
                        then go (p `plusPtr` 1)
                        else f (p `plusPtr` 1) (fromIntegral v)
            in look True sp $ \ !aNext !aValue ->
               look True aNext $ \ !bNext !bValue ->
                 if aValue == done || bValue == done
                 then finish n
                 else
                    look False bNext $ \ !cNext !cValue ->
                    look False cNext $ \ !dNext !dValue -> do
                      let w = (aValue `shiftL` 18) .|. (bValue `shiftL` 12) .|.
                              (cValue `shiftL` 6) .|. dValue
                      poke8 dp $ fromIntegral (w `shiftR` 16)
                      if cValue == done
                        then finish (n + 1)
                        else do
                          poke8 (dp `plusPtr` 1) $ fromIntegral (w `shiftR` 8)
                          if dValue == done
                            then finish (n + 2)
                            else do
                              poke8 (dp `plusPtr` 2) $ fromIntegral w
                              fill (dp `plusPtr` 3) dNext (n+3)
      withForeignPtr dfp $ \dptr ->
        fill dptr (sptr `plusPtr` soff) 0
  where dlen = ((slen + 3) `div` 4) * 3

x :: Integral a => a
x = 255
{-# INLINE x #-}

done :: Integral a => a
done = 99
{-# INLINE done #-}

-- This takes a list of ByteStrings, and returns a list in which each
-- (apart from possibly the last) has length that is a multiple of n
reChunkIn :: Int -> [ByteString] -> [ByteString]
reChunkIn !n = go
  where
    go [] = []
    go (y : ys) = case B.length y `divMod` n of
                    (_, 0) -> y : go ys
                    (d, _) -> case B.splitAt (d * n) y of
                                (prefix, suffix) -> prefix : fixup suffix ys
    fixup acc [] = [acc]
    fixup acc (z : zs) = case B.splitAt (n - B.length acc) z of
                           (prefix, suffix) ->
                             let acc' = acc `B.append` prefix
                             in if B.length acc' == n
                                then let zs' = if B.null suffix
                                               then          zs
                                               else suffix : zs
                                     in acc' : go zs'
                                else -- suffix must be null
                                    fixup acc' zs
