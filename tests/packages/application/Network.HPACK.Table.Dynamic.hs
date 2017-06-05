{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Table/Dynamic.hs" #-}



































































{-# LANGUAGE TupleSections, RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE BangPatterns, CPP #-}

module Network.HPACK.Table.Dynamic (
    DynamicTable(..)
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , renewDynamicTable
  , huffmanDecoder
  , printDynamicTable
  , isDynamicTableEmpty
  , isSuitableSize
  , TableSizeAction(..)
  , needChangeTableSize
  , setLimitForEncoding
  , resetLimitForEncoding
  , insertEntry
  , toDynamicEntry
  , CodeInfo(..)
  , clearDynamicTable
  , withDynamicTableForEncoding
  , withDynamicTableForDecoding
  , toIndexedEntry
  , fromHIndexToIndex
  , getRevIndex
  ) where

import Control.Exception (bracket, throwIO)
import Control.Monad (forM, when, (>=>))
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Foreign.Marshal.Alloc
import Network.HPACK.Huffman
import Network.HPACK.Table.Entry
import Network.HPACK.Table.RevIndex
import Network.HPACK.Table.Static
import Network.HPACK.Types

----------------------------------------------------------------

-- For decoder
{-# INLINE toIndexedEntry #-}
toIndexedEntry :: DynamicTable -> Index -> IO Entry
toIndexedEntry dyntbl idx
  | idx <= 0               = throwIO $ IndexOverrun idx
  | idx <= staticTableSize = return $! toStaticEntry idx
  | otherwise              = toDynamicEntry dyntbl idx

-- For encoder
{-# INLINE fromHIndexToIndex #-}
fromHIndexToIndex :: DynamicTable -> HIndex -> IO Index
fromHIndexToIndex _ (SIndex idx) = return idx
fromHIndexToIndex DynamicTable{..} (DIndex didx) = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    x <- adj maxN (didx - off)
    return $! x + staticTableSize

----------------------------------------------------------------

type Table = IOArray Index Entry

{-
        offset
        v
   +-+-+-+-+-+-+-+-+
   | | | |z|y|x| | |
   +-+-+-+-+-+-+-+-+
          1 2 3      (numOfEntries = 3)

After insertion:

      offset
      v
   +-+-+-+-+-+-+-+-+
   | | |w|z|y|x| | |
   +-+-+-+-+-+-+-+-+
        1 2 3 4      (numOfEntries = 4)
-}

data CodeInfo =
    EncodeInfo !RevIndex -- Reverse index
               -- The value informed by SETTINGS_HEADER_TABLE_SIZE.
               -- If 'Nothing', dynamic table size update is not necessary.
               -- Otherwise, dynamic table size update is sent
               -- and this value should be set to 'Nothing'.
               !(IORef (Maybe Size))
  | DecodeInfo !HuffmanDecoding
               !(IORef Size)  -- The limit size
               !(IO ())       -- Action to free the buffer

-- | Type for dynamic table.
data DynamicTable = DynamicTable {
    codeInfo :: !CodeInfo
  -- | An array
  , circularTable :: !(IORef Table)
  -- | Start point
  , offset :: !(IORef Index)
  -- | The current number of entries
  , numOfEntries :: !(IORef Int)
  -- | The size of the array
  , maxNumOfEntries :: !(IORef Int)
  -- | The current dynamic table size (defined in HPACK)
  , dynamicTableSize :: !(IORef Size)
  -- | The max dynamic table size (defined in HPACK)
  , maxDynamicTableSize :: !(IORef Size)
  }

{-# INLINE adj #-}
adj :: Int -> Int -> IO Int
adj maxN x
  | maxN == 0 = throwIO TooSmallTableSize
  | otherwise = let !ret = (x + maxN) `mod` maxN
                in return ret

huffmanDecoder :: DynamicTable -> HuffmanDecoding
huffmanDecoder DynamicTable{..} = dec
  where
    DecodeInfo dec _ _ = codeInfo

----------------------------------------------------------------

-- | Printing 'DynamicTable'.
printDynamicTable :: DynamicTable -> IO ()
printDynamicTable DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    let !beg = off + 1
        !end = off + n
    tbl <- readIORef circularTable
    es <- mapM (adj maxN >=> unsafeRead tbl) [beg .. end]
    let !ts = zip [1..] es
    mapM_ printEntry ts
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    putStrLn $ "      Table size: " ++ show dsize ++ "/" ++ show maxdsize

printEntry :: (Index,Entry) -> IO ()
printEntry (i,e) = do
    putStr "[ "
    putStr $ show i
    putStr "] (s = "
    putStr $ show $ entrySize e
    putStr ") "
    BS.putStr $ entryHeaderName e
    putStr ": "
    BS.putStrLn $ entryHeaderValue e

----------------------------------------------------------------

isDynamicTableEmpty :: DynamicTable -> IO Bool
isDynamicTableEmpty DynamicTable{..} = do
    n <- readIORef numOfEntries
    return $! n == 0

isSuitableSize :: Size -> DynamicTable -> IO Bool
isSuitableSize siz DynamicTable{..} = do
    let DecodeInfo _ limref _ = codeInfo
    lim <- readIORef limref
    return $! siz <= lim

data TableSizeAction = Keep | Change !Size | Ignore !Size

needChangeTableSize :: DynamicTable -> IO TableSizeAction
needChangeTableSize DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    mlim <- readIORef limref
    maxsiz <- readIORef maxDynamicTableSize
    return $ case mlim of
        Nothing          -> Keep
        Just lim
          | lim < maxsiz -> Change lim
          | otherwise    -> Ignore maxsiz

-- | When SETTINGS_HEADER_TABLE_SIZE is received from a peer,
--   its value should be set by this function.
setLimitForEncoding :: Size -> DynamicTable -> IO ()
setLimitForEncoding siz DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    writeIORef limref $ Just siz

resetLimitForEncoding :: DynamicTable -> IO ()
resetLimitForEncoding DynamicTable{..} = do
    let EncodeInfo _ limref = codeInfo
    writeIORef limref Nothing

----------------------------------------------------------------

-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding :: Size -- ^ The dynamic table size
                           -> IO DynamicTable
newDynamicTableForEncoding maxsiz = do
    rev <- newRevIndex
    lim <- newIORef Nothing
    let !info = EncodeInfo rev lim
    newDynamicTable maxsiz info

-- | Creating 'DynamicTable' for decoding.
newDynamicTableForDecoding :: Size -- ^ The dynamic table size
                           -> Size -- ^ The size of temporary buffer for Huffman decoding
                           -> IO DynamicTable
newDynamicTableForDecoding maxsiz huftmpsiz = do
    lim <- newIORef maxsiz
    buf <- mallocBytes huftmpsiz
    let !decoder = decode buf huftmpsiz
        !clear = free buf
        !info = DecodeInfo decoder lim clear
    newDynamicTable maxsiz info

newDynamicTable :: Size -> CodeInfo -> IO DynamicTable
newDynamicTable maxsiz info = do
    tbl <- newArray (0,end) dummyEntry
    DynamicTable info <$> newIORef tbl     -- circularTable
                      <*> newIORef end     -- offset
                      <*> newIORef 0       -- numOfEntries
                      <*> newIORef maxN    -- maxNumOfEntries
                      <*> newIORef 0       -- dynamicTableSize
                      <*> newIORef maxsiz  -- maxDynamicTableSize
  where
    !maxN = maxNumbers maxsiz
    !end = maxN - 1

-- | Renewing 'DynamicTable' with necessary entries copied.
renewDynamicTable :: Size -> DynamicTable -> IO ()
renewDynamicTable maxsiz dyntbl@DynamicTable{..} = do
    renew <- shouldRenew dyntbl maxsiz
    when renew $ do
        !entries <- getEntries dyntbl
        let !maxN = maxNumbers maxsiz
            !end = maxN - 1
        newtbl <- newArray (0,end) dummyEntry
        writeIORef circularTable newtbl
        writeIORef offset end
        writeIORef numOfEntries 0
        writeIORef maxNumOfEntries maxN
        writeIORef dynamicTableSize 0
        writeIORef maxDynamicTableSize maxsiz
        case codeInfo of
            EncodeInfo rev _ -> renewRevIndex rev
            _                -> return ()
        copyEntries dyntbl entries

getEntries :: DynamicTable -> IO [Entry]
getEntries DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    table <- readIORef circularTable
    let readTable i = adj maxN (off + i) >>= unsafeRead table
    forM [1 .. n] readTable

copyEntries :: DynamicTable -> [Entry] -> IO ()
copyEntries _                           [] = return ()
copyEntries dyntbl@DynamicTable{..} (e:es) = do
    dsize <- readIORef dynamicTableSize
    maxdsize <- readIORef maxDynamicTableSize
    when (dsize + entrySize e <= maxdsize) $ do
        insertEnd e dyntbl
        copyEntries dyntbl es

-- | Is the size of 'DynamicTable' really changed?
shouldRenew :: DynamicTable -> Size -> IO Bool
shouldRenew DynamicTable{..} maxsiz = do
    maxdsize <- readIORef maxDynamicTableSize
    return $! maxdsize /= maxsiz

----------------------------------------------------------------

-- | Creating 'DynamicTable' for encoding,
--   performing the action and
--   clearing the 'DynamicTable'.
withDynamicTableForEncoding :: Size -- ^ The dynamic table size
                            -> (DynamicTable -> IO a)
                            -> IO a
withDynamicTableForEncoding maxsiz action =
    bracket (newDynamicTableForEncoding maxsiz) clearDynamicTable action

-- | Creating 'DynamicTable' for decoding,
--   performing the action and
--   clearing the 'DynamicTable'.
withDynamicTableForDecoding :: Size -- ^ The dynamic table size
                            -> Size -- ^ The size of temporary buffer for Huffman
                            -> (DynamicTable -> IO a)
                            -> IO a
withDynamicTableForDecoding maxsiz huftmpsiz action =
    bracket (newDynamicTableForDecoding maxsiz huftmpsiz) clearDynamicTable action

-- | Clearing 'DynamicTable'.
--   Currently, this frees the temporary buffer for Huffman decoding.
clearDynamicTable :: DynamicTable -> IO ()
clearDynamicTable DynamicTable{..} = case codeInfo of
    EncodeInfo _ _       -> return ()
    DecodeInfo _ _ clear -> clear

----------------------------------------------------------------

-- | Inserting 'Entry' to 'DynamicTable'.
--   New 'DynamicTable', the largest new 'Index'
--   and a set of dropped OLD 'Index'
--   are returned.
insertEntry :: Entry -> DynamicTable -> IO ()
insertEntry e dyntbl@DynamicTable{..} = do
    insertFront e dyntbl
    es <- adjustTableSize dyntbl
    case codeInfo of
        EncodeInfo rev _ -> deleteRevIndexList es rev
        _                -> return ()

insertFront :: Entry -> DynamicTable -> IO ()
insertFront e DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    dsize <- readIORef dynamicTableSize
    table <- readIORef circularTable
    let i = off
        !dsize' = dsize + entrySize e
    !off' <- adj maxN (off - 1)
    unsafeWrite table i e
    writeIORef offset off'
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    case codeInfo of
        EncodeInfo rev _ -> insertRevIndex e (DIndex i) rev
        _                -> return ()

adjustTableSize :: DynamicTable -> IO [Entry]
adjustTableSize dyntbl@DynamicTable{..} = adjust []
  where
    adjust :: [Entry] -> IO [Entry]
    adjust !es = do
        dsize <- readIORef dynamicTableSize
        maxdsize <- readIORef maxDynamicTableSize
        if dsize <= maxdsize then
            return es
          else do
            e <- removeEnd dyntbl
            adjust (e:es)

----------------------------------------------------------------

insertEnd :: Entry -> DynamicTable -> IO ()
insertEnd e DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    dsize <- readIORef dynamicTableSize
    table <- readIORef circularTable
    !i <- adj maxN (off + n + 1)
    let !dsize' = dsize + entrySize e
    unsafeWrite table i e
    writeIORef numOfEntries $ n + 1
    writeIORef dynamicTableSize dsize'
    case codeInfo of
        EncodeInfo rev _ -> insertRevIndex e (DIndex i) rev
        _                -> return ()

----------------------------------------------------------------

removeEnd :: DynamicTable -> IO Entry
removeEnd DynamicTable{..} = do
    maxN <- readIORef maxNumOfEntries
    off <- readIORef offset
    n <- readIORef numOfEntries
    !i <- adj maxN (off + n)
    table <- readIORef circularTable
    e <- unsafeRead table i
    unsafeWrite table i dummyEntry -- let the entry GCed
    dsize <- readIORef dynamicTableSize
    let !dsize' = dsize - entrySize e
    writeIORef numOfEntries (n - 1)
    writeIORef dynamicTableSize dsize'
    return e

----------------------------------------------------------------

{-# INLINE toDynamicEntry #-}
toDynamicEntry :: DynamicTable -> Index -> IO Entry
toDynamicEntry DynamicTable{..} idx = do
    !maxN <- readIORef maxNumOfEntries
    !off <- readIORef offset
    !n <- readIORef numOfEntries
    when (idx > n + staticTableSize) $ throwIO $ IndexOverrun idx
    !didx <- adj maxN (idx + off - staticTableSize)
    !table <- readIORef circularTable
    unsafeRead table didx

----------------------------------------------------------------

{-# INLINE getRevIndex #-}
getRevIndex :: DynamicTable-> RevIndex
getRevIndex DynamicTable{..} = rev
  where
    EncodeInfo rev _ = codeInfo
