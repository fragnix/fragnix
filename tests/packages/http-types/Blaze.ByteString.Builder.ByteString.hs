{-# LINE 1 "./Blaze/ByteString/Builder/ByteString.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Blaze/ByteString/Builder/ByteString.hs" #-}
{-# LINE 1 "./Blaze/ByteString/Builder/ByteString.hs" #-}
{-# LANGUAGE CPP, BangPatterns, OverloadedStrings #-}





{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Blaze.ByteString.Builder.ByteString
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- 'Write's and 'Builder's for strict and lazy bytestrings.
--
-- We assume the following qualified imports in order to differentiate between
-- strict and lazy bytestrings in the code examples.
--
-- > import qualified Data.ByteString      as S
-- > import qualified Data.ByteString.Lazy as L
--
module Blaze.ByteString.Builder.ByteString
    (
    -- * Strict bytestrings
      writeByteString
    , fromByteString
    , fromByteStringWith
    , copyByteString
    , insertByteString

    -- * Lazy bytestrings
    , fromLazyByteString
    , fromLazyByteStringWith
    , copyLazyByteString
    , insertLazyByteString

    ) where

import           Blaze.ByteString.Builder.Internal hiding (insertByteString)
import qualified Blaze.ByteString.Builder.Internal as I   (insertByteString)





import Foreign                   (unsafeForeignPtrToPtr, withForeignPtr, touchForeignPtr, copyBytes, plusPtr, minusPtr)


import Data.Monoid

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L





import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L



------------------------------------------------------------------------------
-- Strict ByteStrings
------------------------------------------------------------------------------

-- | Write a strict 'S.ByteString' to a buffer.
--
writeByteString :: S.ByteString -> Write
writeByteString bs = exactWrite l io
  where
  (fptr, o, l) = S.toForeignPtr bs
  io pf = withForeignPtr fptr $ \p -> copyBytes pf (p `plusPtr` o) l
{-# INLINE writeByteString #-}

-- | Smart serialization of a strict bytestring.
--
-- @'fromByteString' = 'fromByteStringWith' 'defaultMaximalCopySize'@
--
-- Use this function to serialize strict bytestrings. It guarantees an
-- average chunk size of 4kb, which has been shown to be a reasonable size in
-- benchmarks. Note that the check whether to copy or to insert is (almost)
-- free as the builder performance is mostly memory-bound.
--
-- If you statically know that copying or inserting the strict bytestring is
-- always the best choice, then you can use the 'copyByteString' or
-- 'insertByteString' functions.
--
fromByteString :: S.ByteString -> Builder
fromByteString = fromByteStringWith defaultMaximalCopySize
{-# INLINE fromByteString #-}


-- | @fromByteStringWith maximalCopySize bs@ serializes the strict bytestring
-- @bs@ according to the following rules.
--
--   [@S.length bs <= maximalCopySize@:] @bs@ is copied to the output buffer.
--
--   [@S.length bs >  maximalCopySize@:] @bs@ the output buffer is flushed and
--   @bs@ is inserted directly as separate chunk in the output stream.
--
-- These rules guarantee that average chunk size in the output stream is at
-- least half the @maximalCopySize@.
--
fromByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                   -> S.ByteString -- ^ Strict 'S.ByteString' to serialize.
                   -> Builder      -- ^ Resulting 'Builder'.
fromByteStringWith maxCopySize =
    \bs -> fromBuildStepCont $ step bs
  where
    step !bs !k br@(BufRange !op _)
      | maxCopySize < S.length bs = return $ I.insertByteString op bs k
      | otherwise                 = copyByteStringStep bs k br
{-# INLINE fromByteStringWith #-}

-- | @copyByteString bs@ serialize the strict bytestring @bs@ by copying it to
-- the output buffer.
--
-- Use this function to serialize strict bytestrings that are statically known
-- to be smallish (@<= 4kb@).
--
copyByteString :: S.ByteString -> Builder
copyByteString = \bs -> fromBuildStepCont $ copyByteStringStep bs
{-# INLINE copyByteString #-}

copyByteStringStep :: S.ByteString
                   -> (BufRange -> IO (BuildSignal a))
                   -> (BufRange -> IO (BuildSignal a))
copyByteStringStep (S.PS ifp ioff isize) !k =
    goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
  where
    !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
    goBS !ip !(BufRange op ope)
      | inpRemaining <= outRemaining = do
          copyBytes op ip inpRemaining
          touchForeignPtr ifp -- input consumed: OK to release from here
          let !br' = BufRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyBytes op ip outRemaining
          let !ip' = ip `plusPtr` outRemaining
          return $ bufferFull 1 ope (goBS ip')
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe `minusPtr` ip
{-# INLINE copyByteStringStep #-}

-- | @insertByteString bs@ serializes the strict bytestring @bs@ by inserting
-- it directly as a chunk of the output stream.
--
-- Note that this implies flushing the output buffer; even if it contains just
-- a single byte. Hence, you should use this operation only for large (@> 8kb@)
-- bytestrings, as otherwise the resulting output stream may be too fragmented
-- to be processed efficiently.
--
insertByteString :: S.ByteString -> Builder
insertByteString =
    \bs -> fromBuildStepCont $ step bs
  where
    step !bs !k !(BufRange op _) = return $ I.insertByteString op bs k
{-# INLINE insertByteString #-}


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | /O(n)/. Smart serialization of a lazy bytestring.
--
-- @'fromLazyByteString' = 'fromLazyByteStringWith' 'defaultMaximalCopySize'@
--
-- Use this function to serialize lazy bytestrings. It guarantees an average
-- chunk size of 4kb, which has been shown to be a reasonable size in
-- benchmarks. Note that the check whether to copy or to insert is (almost)
-- free as the builder performance is mostly memory-bound.
--
-- If you statically know that copying or inserting /all/ chunks of the lazy
-- bytestring is always the best choice, then you can use the
-- 'copyLazyByteString' or 'insertLazyByteString' functions.
--
fromLazyByteString :: L.ByteString -> Builder
fromLazyByteString = fromLazyByteStringWith defaultMaximalCopySize
{-# INLINE fromLazyByteString #-}

-- | /O(n)/. Serialize a lazy bytestring chunk-wise according to the same rules
-- as in 'fromByteStringWith'.
--
-- Semantically, it holds that
--
-- >   fromLazyByteStringWith maxCopySize
-- > = mconcat . map (fromByteStringWith maxCopySize) . L.toChunks
--
-- However, the left-hand-side is much more efficient, as it moves the
-- end-of-buffer pointer out of the inner loop and provides the compiler with
-- more strictness information.
--
fromLazyByteStringWith :: Int          -- ^ Maximal number of bytes to copy.
                       -> L.ByteString -- ^ Lazy 'L.ByteString' to serialize.
                       -> Builder      -- ^ Resulting 'Builder'.
fromLazyByteStringWith maxCopySize =
  L.foldrChunks (\bs b -> fromByteStringWith maxCopySize bs `mappend` b) mempty
{-# INLINE fromLazyByteStringWith #-}


-- | /O(n)/. Serialize a lazy bytestring by copying /all/ chunks sequentially
-- to the output buffer.
--
-- See 'copyByteString' for usage considerations.
--
copyLazyByteString :: L.ByteString -> Builder
copyLazyByteString =
  L.foldrChunks (\bs b -> copyByteString bs `mappend` b) mempty
{-# INLINE copyLazyByteString #-}

-- | /O(n)/. Serialize a lazy bytestring by inserting /all/ its chunks directly
-- into the output stream.
--
-- See 'insertByteString' for usage considerations.
--
-- For library developers, see the 'ModifyChunks' build signal, if you
-- need an /O(1)/ lazy bytestring insert based on difference lists.
--
insertLazyByteString :: L.ByteString -> Builder
insertLazyByteString =
  L.foldrChunks (\bs b -> insertByteString bs `mappend` b) mempty
{-# INLINE insertLazyByteString #-}

