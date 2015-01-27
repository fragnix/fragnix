{-# LINE 1 "Data/Streaming/Text.hs" #-}
# 1 "Data/Streaming/Text.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1



































































































































































# 8 "<command-line>" 2
# 1 "Data/Streaming/Text.hs"
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE UnliftedFFITypes           #-}

--
-- Module      : Data.Text.Lazy.Encoding.Fusion
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fusible 'Stream'-oriented functions for converting between lazy
-- 'Text' and several common encodings.

-- | Provides a stream-based approach to decoding Unicode data. Each function
-- below works the same way: you give it a chunk of data, and it gives back a
-- @DecodeResult@. If the parse was a success, then you get a chunk of @Text@
-- (possibly empty) and a continuation parsing function. If the parse was a
-- failure, you get a chunk of successfully decoded @Text@ (possibly empty) and
-- the unconsumed bytes.
--
-- In order to indicate end of stream, you pass an empty @ByteString@ to the
-- decode function. This call may result in a failure, if there were unused
-- bytes left over from a previous step which formed part of a code sequence.
module Data.Streaming.Text
    (
    -- * Streaming
      decodeUtf8
    , decodeUtf8Pure
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- * Type
    , DecodeResult (..)
    ) where

import           Control.Monad.ST                  (ST, runST)
import           Control.Monad.ST.Unsafe           (unsafeIOToST, unsafeSTToIO)
import           Data.Bits                         ((.|.))
import qualified Data.ByteString                   as B
import           Data.ByteString.Internal          (ByteString (PS))
import qualified Data.ByteString.Unsafe            as B
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Array                   as A
import           Data.Text.Internal                (text)
import qualified Data.Text.Internal.Encoding.Utf16 as U16
import qualified Data.Text.Internal.Encoding.Utf32 as U32
import qualified Data.Text.Internal.Encoding.Utf8  as U8
import           Data.Text.Internal.Unsafe.Char    (unsafeChr, unsafeChr32,
                                                    unsafeChr8)
import           Data.Text.Internal.Unsafe.Char    (unsafeWrite)
import           Data.Text.Internal.Unsafe.Shift   (shiftL)
import           Data.Word                         (Word32, Word8)
import           Foreign.C.Types                   (CSize (..))
import           Foreign.ForeignPtr                (withForeignPtr)
import           Foreign.Marshal.Utils             (with)
import           Foreign.Ptr                       (Ptr, minusPtr, nullPtr,
                                                    plusPtr)
import           Foreign.Storable                  (Storable, peek, poke)
import           GHC.Base                          (MutableByteArray#)

data S = S0
       | S1 {-# UNPACK #-} !Word8
       | S2 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
       | S3 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving Show

data DecodeResult
    = DecodeResultSuccess !Text !(B.ByteString -> DecodeResult)
    | DecodeResultFailure !Text !B.ByteString

toBS :: S -> B.ByteString
toBS S0 = B.empty
toBS (S1 a) = B.pack [a]
toBS (S2 a b) = B.pack [a, b]
toBS (S3 a b c) = B.pack [a, b, c]
{-# INLINE toBS #-}

getText :: Int -> A.MArray s -> ST s Text
getText j marr = do
    arr <- A.unsafeFreeze marr
    return $! text arr 0 j
{-# INLINE getText #-}


# 1 "include/text_cbits.h" 1










# 99 "Data/Streaming/Text.hs" 2

foreign import ccall unsafe "_hs_streaming_commons_decode_utf8_state" c_decode_utf8_with_state
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr (Ptr Word8) -> Ptr Word8
    -> Ptr CodePoint -> Ptr DecoderState -> IO (Ptr Word8)

newtype CodePoint = CodePoint Word32 deriving (Eq, Show, Num, Storable)
newtype DecoderState = DecoderState Word32 deriving (Eq, Show, Num, Storable)

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using
-- UTF-8 encoding.
decodeUtf8 :: B.ByteString -> DecodeResult
decodeUtf8 = decodeChunk B.empty 0 0
 where
  decodeChunkCheck :: B.ByteString -> CodePoint -> DecoderState -> B.ByteString -> DecodeResult
  decodeChunkCheck bsOld codepoint state bs
    | B.null bs =
        if B.null bsOld
            then DecodeResultSuccess T.empty decodeUtf8
            else DecodeResultFailure T.empty bsOld
    | otherwise = decodeChunk bsOld codepoint state bs
  -- We create a slightly larger than necessary buffer to accommodate a
  -- potential surrogate pair started in the last buffer
  decodeChunk :: B.ByteString -> CodePoint -> DecoderState -> B.ByteString -> DecodeResult
  decodeChunk bsOld codepoint0 state0 bs@(PS fp off len) =
    runST $ (unsafeIOToST . decodeChunkToBuffer) =<< A.new (len+1)
   where
    decodeChunkToBuffer :: A.MArray s -> IO DecodeResult
    decodeChunkToBuffer dest = withForeignPtr fp $ \ptr ->
      with (0::CSize) $ \destOffPtr ->
      with codepoint0 $ \codepointPtr ->
      with state0 $ \statePtr ->
      with nullPtr $ \curPtrPtr ->
        let end = ptr `plusPtr` (off + len)
            loop curPtr = do
              poke curPtrPtr curPtr
              _ <- c_decode_utf8_with_state (A.maBA dest) destOffPtr
                         curPtrPtr end codepointPtr statePtr
              state <- peek statePtr
              n <- peek destOffPtr
              chunkText <- unsafeSTToIO $ do
                  arr <- A.unsafeFreeze dest
                  return $! text arr 0 (fromIntegral n)
              lastPtr <- peek curPtrPtr
              let left = lastPtr `minusPtr` curPtr
                  -- The logic here is: if any text was generated, then the
                  -- previous leftovers were completely consumed already.
                  -- If no text was generated, then any leftovers from the
                  -- previous step are still leftovers now.
                  unused
                    | not $ T.null chunkText = B.unsafeDrop left bs
                    | B.null bsOld = bs
                    | otherwise = B.append bsOld bs
              case unused `seq` state of
                12 ->
                  -- We encountered an encoding error
                  return $! DecodeResultFailure chunkText unused
                _ -> do
                  codepoint <- peek codepointPtr
                  return $! DecodeResultSuccess chunkText
                         $! decodeChunkCheck unused codepoint state
        in loop (ptr `plusPtr` off)

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using
-- UTF-8 encoding.
decodeUtf8Pure :: B.ByteString -> DecodeResult
decodeUtf8Pure =
    beginChunk S0
  where
    beginChunk :: S -> B.ByteString -> DecodeResult
    beginChunk s bs | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty (beginChunk S0)
            _  -> DecodeResultFailure T.empty $ toBS s
    beginChunk s0 ps = runST $ do
        let initLen = B.length ps
        marr <- A.new (initLen + 1)
        let start !i !j
                | i >= len = do
                    t <- getText j marr
                    return $! DecodeResultSuccess t (beginChunk S0)
                |                U8.validate1 a       = addChar' 1 (unsafeChr8 a)
                | i + 1 < len && U8.validate2 a b     = addChar' 2 (U8.chr2 a b)
                | i + 2 < len && U8.validate3 a b c   = addChar' 3 (U8.chr3 a b c)
                | i + 3 < len && U8.validate4 a b c d = addChar' 4 (U8.chr4 a b c d)
                | i + 3 < len = do
                    t <- getText j marr
                    return $! DecodeResultFailure t (B.unsafeDrop i ps)
                | i + 2 < len = continue (S3 a b c)
                | i + 1 < len = continue (S2 a b)
                | otherwise   = continue (S1 a)
                  where
                    a = B.unsafeIndex ps i
                    b = B.unsafeIndex ps (i+1)
                    c = B.unsafeIndex ps (i+2)
                    d = B.unsafeIndex ps (i+3)
                    addChar' deltai char = do
                        deltaj <- unsafeWrite marr j char
                        start (i + deltai) (j + deltaj)
                    continue s = do
                        t <- getText j marr
                        return $! DecodeResultSuccess t (beginChunk s)

            checkCont s !i | i >= len = return $! DecodeResultSuccess T.empty (beginChunk s)
            checkCont s !i =
                case s of
                    S0 -> start i 0
                    S1 a
                        | U8.validate2 a x     -> addChar' (U8.chr2 a x)
                        | otherwise            -> checkCont (S2 a x) (i + 1)
                    S2 a b
                        | U8.validate3 a b x   -> addChar' (U8.chr3 a b x)
                        | otherwise            -> checkCont (S3 a b x) (i + 1)
                    S3 a b c
                        | U8.validate4 a b c x -> addChar' (U8.chr4 a b c x)
                    _ -> return $! DecodeResultFailure T.empty
                                $! B.append (toBS s) (B.unsafeDrop i ps)
              where
                x = B.unsafeIndex ps i
                addChar' c = do
                    d <- unsafeWrite marr 0 c
                    start (i + 1) d

        checkCont s0 0
      where
        len = B.length ps
    {-# INLINE beginChunk #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-16 encoding.
decodeUtf16LE :: B.ByteString -> DecodeResult
decodeUtf16LE =
    beginChunk S0
  where
    beginChunk :: S -> B.ByteString -> DecodeResult
    beginChunk s bs | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty (beginChunk S0)
            _  -> DecodeResultFailure T.empty $ toBS s
    beginChunk s0 ps = runST $ do
        let initLen = B.length ps
        marr <- A.new (initLen + 1)
        let start !i !j
                | i >= len = do
                    t <- getText j marr
                    return $! DecodeResultSuccess t (beginChunk S0)
                | i + 1 < len && U16.validate1 x1    = addChar' 2 (unsafeChr x1)
                | i + 3 < len && U16.validate2 x1 x2 = addChar' 4 (U16.chr2 x1 x2)
                | i + 3 < len = do
                    t <- getText j marr
                    return $! DecodeResultFailure t (B.unsafeDrop i ps)
                | i + 2 < len = continue (S3 a b c)
                | i + 1 < len = continue (S2 a b)
                | otherwise   = continue (S1 a)
                  where
                    a = B.unsafeIndex ps i
                    b = B.unsafeIndex ps (i+1)
                    c = B.unsafeIndex ps (i+2)
                    d = B.unsafeIndex ps (i+3)
                    x1   = combine a b
                    x2   = combine c d
                    addChar' deltai char = do
                        deltaj <- unsafeWrite marr j char
                        start (i + deltai) (j + deltaj)
                    continue s = do
                        t <- getText j marr
                        return $! DecodeResultSuccess t (beginChunk s)

            checkCont s !i | i >= len = return $! DecodeResultSuccess T.empty (beginChunk s)
            checkCont s !i =
                case s of
                    S0 -> start i 0
                    S1 a ->
                        let x1 = combine a x
                         in if U16.validate1 x1
                                then addChar' (unsafeChr x1)
                                else checkCont (S2 a x) (i + 1)
                    S2 a b -> checkCont (S3 a b x) (i + 1)
                    S3 a b c ->
                        let x1 = combine a b
                            x2 = combine c x
                         in if U16.validate2 x1 x2
                                then addChar' (U16.chr2 x1 x2)
                                else return $! DecodeResultFailure T.empty
                                            $! B.append (toBS s) (B.unsafeDrop i ps)
              where
                x = B.unsafeIndex ps i
                addChar' c = do
                    d <- unsafeWrite marr 0 c
                    start (i + 1) d

        checkCont s0 0
      where
        len = B.length ps
        combine w1 w2 = fromIntegral w1 .|. (fromIntegral w2 `shiftL` 8)
    {-# INLINE beginChunk #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-16 encoding.
decodeUtf16BE :: B.ByteString -> DecodeResult
decodeUtf16BE =
    beginChunk S0
  where
    beginChunk :: S -> B.ByteString -> DecodeResult
    beginChunk s bs | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty (beginChunk S0)
            _  -> DecodeResultFailure T.empty $ toBS s
    beginChunk s0 ps = runST $ do
        let initLen = B.length ps
        marr <- A.new (initLen + 1)
        let start !i !j
                | i >= len = do
                    t <- getText j marr
                    return $! DecodeResultSuccess t (beginChunk S0)
                | i + 1 < len && U16.validate1 x1    = addChar' 2 (unsafeChr x1)
                | i + 3 < len && U16.validate2 x1 x2 = addChar' 4 (U16.chr2 x1 x2)
                | i + 3 < len = do
                    t <- getText j marr
                    return $! DecodeResultFailure t (B.unsafeDrop i ps)
                | i + 2 < len = continue (S3 a b c)
                | i + 1 < len = continue (S2 a b)
                | otherwise   = continue (S1 a)
                  where
                    a = B.unsafeIndex ps i
                    b = B.unsafeIndex ps (i+1)
                    c = B.unsafeIndex ps (i+2)
                    d = B.unsafeIndex ps (i+3)
                    x1   = combine a b
                    x2   = combine c d
                    addChar' deltai char = do
                        deltaj <- unsafeWrite marr j char
                        start (i + deltai) (j + deltaj)
                    continue s = do
                        t <- getText j marr
                        return $! DecodeResultSuccess t (beginChunk s)

            checkCont s !i | i >= len = return $! DecodeResultSuccess T.empty (beginChunk s)
            checkCont s !i =
                case s of
                    S0 -> start i 0
                    S1 a ->
                        let x1 = combine a x
                         in if U16.validate1 x1
                                then addChar' (unsafeChr x1)
                                else checkCont (S2 a x) (i + 1)
                    S2 a b -> checkCont (S3 a b x) (i + 1)
                    S3 a b c ->
                        let x1 = combine a b
                            x2 = combine c x
                         in if U16.validate2 x1 x2
                                then addChar' (U16.chr2 x1 x2)
                                else return $! DecodeResultFailure T.empty
                                            $! B.append (toBS s) (B.unsafeDrop i ps)
              where
                x = B.unsafeIndex ps i
                addChar' c = do
                    d <- unsafeWrite marr 0 c
                    start (i + 1) d

        checkCont s0 0
      where
        len = B.length ps
        combine w1 w2 = (fromIntegral w1 `shiftL` 8) .|. fromIntegral w2
    {-# INLINE beginChunk #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using little
-- endian UTF-32 encoding.
decodeUtf32LE :: B.ByteString -> DecodeResult
decodeUtf32LE =
    beginChunk S0
  where
    beginChunk :: S -> B.ByteString -> DecodeResult
    beginChunk s bs | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty (beginChunk S0)
            _  -> DecodeResultFailure T.empty $ toBS s
    beginChunk s0 ps = runST $ do
        let initLen = B.length ps `div` 2
        marr <- A.new (initLen + 1)
        let start !i !j
                | i >= len = do
                    t <- getText j marr
                    return $! DecodeResultSuccess t (beginChunk S0)
                | i + 3 < len && U32.validate x1 = addChar' 4 (unsafeChr32 x1)
                | i + 3 < len = do
                    t <- getText j marr
                    return $! DecodeResultFailure t (B.unsafeDrop i ps)
                | i + 2 < len = continue (S3 a b c)
                | i + 1 < len = continue (S2 a b)
                | otherwise   = continue (S1 a)
                  where
                    a = B.unsafeIndex ps i
                    b = B.unsafeIndex ps (i+1)
                    c = B.unsafeIndex ps (i+2)
                    d = B.unsafeIndex ps (i+3)
                    x1   = combine a b c d
                    addChar' deltai char = do
                        deltaj <- unsafeWrite marr j char
                        start (i + deltai) (j + deltaj)
                    continue s = do
                        t <- getText j marr
                        return $! DecodeResultSuccess t (beginChunk s)

            checkCont s !i | i >= len = return $! DecodeResultSuccess T.empty (beginChunk s)
            checkCont s !i =
                case s of
                    S0 -> start i 0
                    S1 a -> checkCont (S2 a x) (i + 1)
                    S2 a b -> checkCont (S3 a b x) (i + 1)
                    S3 a b c ->
                        let x1 = combine a b c x
                         in if U32.validate x1
                                then addChar' (unsafeChr32 x1)
                                else return $! DecodeResultFailure T.empty
                                            $! B.append (toBS s) (B.unsafeDrop i ps)
              where
                x = B.unsafeIndex ps i
                addChar' c = do
                    d <- unsafeWrite marr 0 c
                    start (i + 1) d

        checkCont s0 0
      where
        len = B.length ps
        combine w1 w2 w3 w4 =
                shiftL (fromIntegral w4) 24
            .|. shiftL (fromIntegral w3) 16
            .|. shiftL (fromIntegral w2) 8
            .|.        (fromIntegral w1)
    {-# INLINE beginChunk #-}

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using big
-- endian UTF-32 encoding.
decodeUtf32BE :: B.ByteString -> DecodeResult
decodeUtf32BE =
    beginChunk S0
  where
    beginChunk :: S -> B.ByteString -> DecodeResult
    beginChunk s bs | B.null bs =
        case s of
            S0 -> DecodeResultSuccess T.empty (beginChunk S0)
            _  -> DecodeResultFailure T.empty $ toBS s
    beginChunk s0 ps = runST $ do
        let initLen = B.length ps `div` 2
        marr <- A.new (initLen + 1)
        let start !i !j
                | i >= len = do
                    t <- getText j marr
                    return $! DecodeResultSuccess t (beginChunk S0)
                | i + 3 < len && U32.validate x1 = addChar' 4 (unsafeChr32 x1)
                | i + 3 < len = do
                    t <- getText j marr
                    return $! DecodeResultFailure t (B.unsafeDrop i ps)
                | i + 2 < len = continue (S3 a b c)
                | i + 1 < len = continue (S2 a b)
                | otherwise   = continue (S1 a)
                  where
                    a = B.unsafeIndex ps i
                    b = B.unsafeIndex ps (i+1)
                    c = B.unsafeIndex ps (i+2)
                    d = B.unsafeIndex ps (i+3)
                    x1   = combine a b c d
                    addChar' deltai char = do
                        deltaj <- unsafeWrite marr j char
                        start (i + deltai) (j + deltaj)
                    continue s = do
                        t <- getText j marr
                        return $! DecodeResultSuccess t (beginChunk s)

            checkCont s !i | i >= len = return $! DecodeResultSuccess T.empty (beginChunk s)
            checkCont s !i =
                case s of
                    S0 -> start i 0
                    S1 a -> checkCont (S2 a x) (i + 1)
                    S2 a b -> checkCont (S3 a b x) (i + 1)
                    S3 a b c ->
                        let x1 = combine a b c x
                         in if U32.validate x1
                                then addChar' (unsafeChr32 x1)
                                else return $! DecodeResultFailure T.empty
                                            $! B.append (toBS s) (B.unsafeDrop i ps)
              where
                x = B.unsafeIndex ps i
                addChar' c = do
                    d <- unsafeWrite marr 0 c
                    start (i + 1) d

        checkCont s0 0
      where
        len = B.length ps
        combine w1 w2 w3 w4 =
                shiftL (fromIntegral w1) 24
            .|. shiftL (fromIntegral w2) 16
            .|. shiftL (fromIntegral w3) 8
            .|.        (fromIntegral w4)
    {-# INLINE beginChunk #-}
