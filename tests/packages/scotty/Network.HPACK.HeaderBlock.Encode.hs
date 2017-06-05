{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/HeaderBlock/Encode.hs" #-}



































































{-# LANGUAGE CPP, BangPatterns, RecordWildCards, OverloadedStrings #-}

module Network.HPACK.HeaderBlock.Encode (
    encodeHeader
  , encodeTokenHeader
  ) where

import Control.Exception (bracket, throwIO)
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Bits (setBit)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, create, memcpy)
import Data.IORef
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Ptr (minusPtr)
import Network.HPACK.Buffer
import qualified Network.HPACK.HeaderBlock.Integer as I
import qualified Network.HPACK.Huffman as Huffman
import Network.HPACK.Table
import Network.HPACK.Token
import Network.HPACK.Types

----------------------------------------------------------------

changeTableSize :: DynamicTable -> WorkingBuffer -> IO ()
changeTableSize dyntbl wbuf = do
    msiz <- needChangeTableSize dyntbl
    case msiz of
        Keep -> return ()
        Change lim -> do
            renewDynamicTable lim dyntbl
            change wbuf lim
        Ignore lim -> do
            resetLimitForEncoding dyntbl
            change wbuf lim

----------------------------------------------------------------

-- | Converting 'HeaderList' to the HPACK format.
--   This function has overhead of allocating/freeing a temporary buffer.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader :: EncodeStrategy
             -> Size -- ^ The size of a temporary buffer.
             -> DynamicTable
             -> HeaderList
             -> IO ByteString -- ^ An HPACK format
encodeHeader stgy siz dyntbl hs = encodeHeader' stgy siz dyntbl hs'
  where
    hs' = map (\(k,v) -> let !t = toToken k in (t,v)) hs


-- | Converting 'HeaderList' to the HPACK format.
--   'BufferOverrun' will be thrown if the temporary buffer is too small.
encodeHeader' :: EncodeStrategy
              -> Size -- ^ The size of a temporary buffer.
              -> DynamicTable
              -> TokenHeaderList
              -> IO ByteString -- ^ An HPACK format
encodeHeader' stgy siz dyntbl hs = bracket (mallocBytes siz) free enc
  where
    enc buf = do
        (hs',len) <- encodeTokenHeader buf siz stgy True dyntbl hs
        case hs' of
            [] -> create len $ \p -> memcpy p buf len
            _  -> throwIO BufferOverrun

----------------------------------------------------------------

-- | Converting 'TokenHeaderList' to the HPACK format directly in the buffer.
--
--   4th argument is relating to dynamic table size update.
--   When calling this function for a new 'TokenHeaderList',
--   it must be 'True'.
--   If 'True' and set by 'setLimitForEncoding',
--   dynamic table size update is generated at the beginning of
--   the HPACK format.
--
--   The return value is a pair of leftover 'TokenHeaderList' and
--   how many bytes are filled in the buffer.
--   If the leftover is empty, the encoding is finished.
--   Otherwise, this function should be called with it again.
--   4th argument must be 'False'.
--
encodeTokenHeader :: Buffer
                  -> BufferSize
                  -> EncodeStrategy
                  -> Bool -- ^ 'True' at the first time, 'False' when continued.
                  -> DynamicTable
                  -> TokenHeaderList
                  -> IO (TokenHeaderList, Int) -- ^ Leftover, filled length
encodeTokenHeader buf siz EncodeStrategy{..} first dyntbl hs0 = do
    wbuf <- newWorkingBuffer buf siz
    when first $ changeTableSize dyntbl wbuf
    let fa = indexedHeaderField dyntbl wbuf useHuffman
        fb = literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf useHuffman
        fc = literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf useHuffman
        fd = literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf useHuffman
        fe = literalHeaderFieldWithoutIndexingNewName dyntbl wbuf useHuffman
        fe' = literalHeaderFieldWithoutIndexingNewName' dyntbl wbuf useHuffman
        rev = getRevIndex dyntbl
        step0 = case compressionAlgo of
            Naive  -> naiveStep  fe'
            Static -> staticStep fa fd fe
            Linear -> linearStep rev fa fb fc fd
    ref1 <- currentOffset wbuf >>= newIORef
    ref2 <- newIORef hs0
    loop wbuf ref1 ref2 step0 hs0 `E.catch` \BufferOverrun -> return ()
    end <- readIORef ref1
    let !len = end `minusPtr` buf
    hs <- readIORef ref2
    return (hs, len)
  where
    loop wbuf ref1 ref2 step hsx = go hsx
      where
        go [] = return ()
        go ((t,v):hs) = do
            _ <- step t v
            currentOffset wbuf >>= writeIORef ref1
            writeIORef ref2 hs
            go hs

----------------------------------------------------------------

naiveStep :: (HeaderName -> HeaderValue -> IO ()) -> Token -> HeaderValue -> IO ()
naiveStep fe t v = fe (tokenFoldedKey t) v

----------------------------------------------------------------

staticStep :: FA -> FD -> FE -> Token -> HeaderValue -> IO ()
staticStep fa fd fe t v = lookupRevIndex' t v fa fd fe

----------------------------------------------------------------

linearStep :: RevIndex -> FA -> FB -> FC -> FD -> Token -> HeaderValue -> IO ()
linearStep rev fa fb fc fd t v = lookupRevIndex t v fa fb fc fd rev

----------------------------------------------------------------

type FA = HIndex -> IO ()
type FB = HeaderValue -> Entry -> HIndex -> IO ()
type FC = HeaderName -> HeaderValue -> Entry -> IO ()
type FD = HeaderValue -> HIndex -> IO ()
type FE = HeaderName -> HeaderValue -> IO ()

-- 6.1.  Indexed Header Field Representation
-- Indexed Header Field
indexedHeaderField
    :: DynamicTable -> WorkingBuffer -> Bool -> FA
indexedHeaderField dyntbl wbuf _ hidx =
    fromHIndexToIndex dyntbl hidx >>= index wbuf

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- Indexed Name
literalHeaderFieldWithIncrementalIndexingIndexedName
    :: DynamicTable -> WorkingBuffer -> Bool -> FB
literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf huff v ent hidx = do
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 6 set01 v
    insertEntry ent dyntbl

-- 6.2.1.  Literal Header Field with Incremental Indexing
-- Literal Header Field with Incremental Indexing -- New Name
literalHeaderFieldWithIncrementalIndexingNewName
    :: DynamicTable -> WorkingBuffer -> Bool -> FC
literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf huff k v ent = do
    newName wbuf huff set01 k v
    insertEntry ent dyntbl

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- Indexed Name
literalHeaderFieldWithoutIndexingIndexedName
    :: DynamicTable -> WorkingBuffer -> Bool -> FD
literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf huff v hidx =
    fromHIndexToIndex dyntbl hidx >>= indexedName wbuf huff 4 set0000 v

-- 6.2.2.  Literal Header Field without Indexing
-- Literal Header Field without Indexing -- New Name
literalHeaderFieldWithoutIndexingNewName
    :: DynamicTable -> WorkingBuffer -> Bool -> FE
literalHeaderFieldWithoutIndexingNewName _ wbuf huff k v =
    newName wbuf huff set0000 k v

literalHeaderFieldWithoutIndexingNewName'
    :: DynamicTable -> WorkingBuffer -> Bool -> HeaderName -> HeaderValue -> IO ()
literalHeaderFieldWithoutIndexingNewName' _ wbuf huff k v =
    newName wbuf huff set0000 k v

----------------------------------------------------------------

{-# INLINE change #-}
change :: WorkingBuffer -> Int -> IO ()
change wbuf i = I.encode wbuf set001 5 i

{-# INLINE index #-}
index :: WorkingBuffer -> Int -> IO ()
index wbuf i = I.encode wbuf set1 7 i

-- Using Huffman encoding
{-# INLINE indexedName #-}
indexedName :: WorkingBuffer -> Bool -> Int -> Setter -> HeaderValue -> Index -> IO ()
indexedName wbuf huff n set v idx = do
    I.encode wbuf set n idx
    encodeString huff v wbuf

-- Using Huffman encoding
{-# INLINE newName #-}
newName :: WorkingBuffer -> Bool -> Setter -> HeaderName -> HeaderValue -> IO ()
newName wbuf huff set k v = do
    writeWord8 wbuf $ set 0
    encodeString huff k wbuf
    encodeString huff v wbuf

----------------------------------------------------------------

type Setter = Word8 -> Word8

-- Assuming MSBs are 0.
set1, set01, set001, set0000, setH :: Setter
set1    x = x `setBit` 7
set01   x = x `setBit` 6
set001  x = x `setBit` 5
-- set0001 x = x `setBit` 4 -- Never indexing
set0000 = id
setH = set1

----------------------------------------------------------------

{-# INLINE encodeString #-}
encodeString :: Bool -> ByteString -> WorkingBuffer -> IO ()
encodeString False bs wbuf = do
    let !len = BS.length bs
    I.encode wbuf id 7 len
    copyByteString wbuf bs
encodeString True  bs wbuf = do
    let !origLen = BS.length bs
        !expectedLen = (origLen `div` 10) * 8 -- 80%: decided by examples
        !expectedIntLen = integerLength expectedLen
    wind wbuf expectedIntLen
    len <- Huffman.encode wbuf bs
    let !intLen = integerLength len
    if origLen < len then do
        wind wbuf (negate (expectedIntLen + len))
        I.encode wbuf id 7 origLen
        copyByteString wbuf bs
      else if intLen == expectedIntLen then do
        wind wbuf (negate (expectedIntLen + len))
        I.encode wbuf setH 7 len
        wind wbuf len
      else do
        let !gap = intLen - expectedIntLen
        shiftLastN wbuf gap len
        wind wbuf (negate (intLen + len))
        I.encode wbuf setH 7 len
        wind wbuf len

-- For 7+:
-- 1 byte:    0 -   126
-- 2 bytes: 127 -   254
-- 3 bytes: 255 - 16510
{-# INLINE integerLength #-}
integerLength :: Int -> Int
integerLength n
    | n <= 126  = 1
    | n <= 254  = 2
    | otherwise = 3
