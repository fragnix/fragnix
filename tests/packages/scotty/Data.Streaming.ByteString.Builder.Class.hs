{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/ByteString/Builder/Class.hs" #-}





















































































{-# LANGUAGE CPP #-}
-- | Typeclass to stream blaze-builder and bytestring(-builder) @Builder@s.
--
-- Since 0.1.10.0
--
module Data.Streaming.ByteString.Builder.Class
    ( StreamingBuilder (..)
    , module Data.Streaming.ByteString.Builder
    ) where

import qualified Data.ByteString.Builder
import qualified Data.ByteString.Builder.Internal
import Data.Monoid (Monoid)

import Data.Streaming.ByteString.Builder hiding (newByteStringBuilderRecv)
import qualified Data.Streaming.ByteString.Builder


-- | Typeclass to stream blaze-builder (< 0.4) and bytestring(-builder) @Builder@s.
-- This is primarily to aid the transition from blaze-builder to bytestring @Builder@s
-- (if using blaze-builder >= 0.4, there is only one instance, since the @Builder@
-- type is shared).
--
-- Since 0.1.10.0
--
class Monoid b => StreamingBuilder b where
    newBuilderRecv :: BufferAllocStrategy -> IO (b -> IO BuilderPopper, BuilderFinish)
    builderFlush   :: b

instance StreamingBuilder Data.ByteString.Builder.Builder where
    newBuilderRecv = Data.Streaming.ByteString.Builder.newByteStringBuilderRecv
    builderFlush   = Data.ByteString.Builder.Internal.flush
