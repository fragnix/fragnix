{-# LINE 1 "System/Log/FastLogger/IO.hs" #-}
# 1 "System/Log/FastLogger/IO.hs"
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
# 1 "System/Log/FastLogger/IO.hs"
{-# LANGUAGE BangPatterns, CPP #-}

module System.Log.FastLogger.IO where


import Data.ByteString.Builder.Extra (Next(..))
import qualified Data.ByteString.Builder.Extra as BBE




import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)
import System.Log.FastLogger.LogStr

type Buffer = Ptr Word8

-- | The type for buffer size of each core.
type BufSize = Int

-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096

getBuffer :: BufSize -> IO Buffer
getBuffer = mallocBytes

freeBuffer :: Buffer -> IO ()
freeBuffer = free


toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop $ BBE.runBuilder builder
  where
    loop writer = do
        (len, next) <- writer buf size
        io buf len
        case next of
             Done -> return ()
             More minSize writer'
               | size < minSize -> error "toBufIOWith: More: minSize"
               | otherwise      -> loop writer'
             Chunk (PS fptr off siz) writer'
               | len == 0  -> loop writer' -- flushing
               | otherwise -> withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz
# 70 "System/Log/FastLogger/IO.hs"

