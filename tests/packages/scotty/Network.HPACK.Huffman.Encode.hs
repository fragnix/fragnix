{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Huffman/Encode.hs" #-}



































































{-# LANGUAGE BangPatterns, CPP, RecordWildCards #-}

module Network.HPACK.Huffman.Encode (
  -- * Huffman encoding
    HuffmanEncoding
  , encode
  , encodeHuffman
  ) where

import Control.Exception (throwIO)
import Control.Monad (when, void)
import Data.Array.Base (unsafeAt)
import Data.Array.IArray (listArray)
import Data.Array.Unboxed (UArray)
import Data.Bits ((.|.), shiftR, shiftL)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.Storable (poke)
import Network.HPACK.Buffer
import Network.HPACK.Huffman.Params (idxEos)
import Network.HPACK.Huffman.Table
import Network.HPACK.Types (BufferOverrun(..))

----------------------------------------------------------------

huffmanLength :: UArray Int Int
huffmanLength = listArray (0,idxEos) $ map length huffmanTable

huffmanCode :: UArray Int Word64
huffmanCode = listArray (0,idxEos) huffmanTable'

----------------------------------------------------------------

-- | Huffman encoding.
type HuffmanEncoding = WorkingBuffer -> ByteString -> IO Int

-- | Huffman encoding.
encode :: HuffmanEncoding
encode dst bs = withReadBuffer bs $ enc dst

-- The maximum length of Huffman code is 30.
-- 40 is enough as a work space.
initialOffset :: Int
initialOffset = 40

shiftForWrite :: Int
shiftForWrite = 32

enc :: WorkingBuffer -> ReadBuffer -> IO Int
enc WorkingBuffer{..} rbuf = do
    beg <- readIORef offset
    end <- go (beg,0,initialOffset)
    writeIORef offset end
    let !len = end `minusPtr` beg
    return len
  where
    go (dst,encoded,off) = do
        !i <- getByte' rbuf
        if i >= 0 then
            copy dst (bond i) >>= go
          else if off == initialOffset then
            return dst
          else do
            let (encoded1,_) = bond idxEos
            write dst encoded1
      where
        {-# INLINE bond #-}
        bond i = (encoded', off')
          where
            !len = huffmanLength `unsafeAt` i
            !code = huffmanCode `unsafeAt` i
            !scode = code `shiftL` (off - len)
            !encoded' = encoded .|. scode
            !off' = off - len
        {-# INLINE write #-}
        write p w = do
            when (p >= limit) $ throwIO BufferOverrun
            let !w8 = fromIntegral (w `shiftR` shiftForWrite) :: Word8
            poke p w8
            let !p' = p `plusPtr` 1
            return p'
        {-# INLINE copy #-}
        copy p (w,o)
          | o > shiftForWrite = return (p,w,o)
          | otherwise = do
              p' <- write p w
              let !w' = w `shiftL` 8
                  !o' = o + 8
              copy p' (w',o')

encodeHuffman :: ByteString -> IO ByteString
encodeHuffman bs = withTemporaryBuffer 4096 $ \wbuf ->
    void $ encode wbuf bs
