{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HTTP2/Encode.hs" #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.Encode (
    encodeFrame
  , encodeFrameChunks
  , encodeFrameHeader
  , encodeFrameHeaderBuf
  , encodeFramePayload
  , EncodeInfo(..)
  , encodeInfo
  ) where

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString, unsafeCreate)
import Data.Word (Word8, Word16, Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

import Network.HTTP2.Types

----------------------------------------------------------------

type Builder = [ByteString] -> [ByteString]

-- | Auxiliary information for frame encoding.
data EncodeInfo = EncodeInfo {
    -- | Flags to be set in a frame header
      encodeFlags    :: !FrameFlags
    -- | Stream id to be set in a frame header
    , encodeStreamId :: !StreamId
    -- | Padding if any. In the case where this value is set but the priority flag is not set, this value gets preference over the priority flag. So, if this value is set, the priority flag is also set.
    , encodePadding  :: !(Maybe Padding)
    } deriving (Show,Read)

----------------------------------------------------------------

-- | A smart builder of 'EncodeInfo'.
--
-- >>> encodeInfo setAck 0
-- EncodeInfo {encodeFlags = 1, encodeStreamId = 0, encodePadding = Nothing}
encodeInfo :: (FrameFlags -> FrameFlags)
           -> Int -- ^ stream identifier
           -> EncodeInfo
encodeInfo set sid = EncodeInfo (set defaultFlags) sid Nothing

----------------------------------------------------------------

-- | Encoding an HTTP/2 frame to 'ByteString'.
-- This function is not efficient enough for high performace
-- program because of the concatenation of 'ByteString'.
--
-- >>> encodeFrame (encodeInfo id 1) (DataFrame "body")
-- "\NUL\NUL\EOT\NUL\NUL\NUL\NUL\NUL\SOHbody"
encodeFrame :: EncodeInfo -> FramePayload -> ByteString
encodeFrame einfo payload = BS.concat $ encodeFrameChunks einfo payload

-- | Encoding an HTTP/2 frame to ['ByteString'].
--   This is suitable for sendMany.
encodeFrameChunks :: EncodeInfo -> FramePayload -> [ByteString]
encodeFrameChunks einfo payload = bs : bss
  where
    ftid = framePayloadToFrameTypeId payload
    bs = encodeFrameHeader ftid header
    (header, bss) = encodeFramePayload einfo payload

-- | Encoding an HTTP/2 frame header.
--   The frame header must be completed.
encodeFrameHeader :: FrameTypeId -> FrameHeader -> ByteString
encodeFrameHeader ftid fhdr = unsafeCreate frameHeaderLength $ encodeFrameHeaderBuf ftid fhdr

-- | Writing an encoded HTTP/2 frame header to the buffer.
--   The length of the buffer must be larger than or equal to 9 bytes.
encodeFrameHeaderBuf :: FrameTypeId -> FrameHeader -> Ptr Word8 -> IO ()
encodeFrameHeaderBuf ftid FrameHeader{..} ptr = do
    poke24 ptr payloadLength
    poke8 ptr 3 typ
    poke8 ptr 4 flags
    poke32 (ptr `plusPtr` 5) sid
  where
    typ = fromFrameTypeId ftid
    sid = fromIntegral streamId

-- | Encoding an HTTP/2 frame payload.
--   This returns a complete frame header and chunks of payload.
encodeFramePayload :: EncodeInfo -> FramePayload -> (FrameHeader, [ByteString])
encodeFramePayload einfo payload = (header, builder [])
  where
    (header, builder) = buildFramePayload einfo payload

----------------------------------------------------------------

buildFramePayload :: EncodeInfo -> FramePayload -> (FrameHeader, Builder)
buildFramePayload einfo (DataFrame body) =
    buildFramePayloadData einfo body
buildFramePayload einfo (HeadersFrame mpri hdr) =
    buildFramePayloadHeaders einfo mpri hdr
buildFramePayload einfo (PriorityFrame pri) =
    buildFramePayloadPriority einfo pri
buildFramePayload einfo (RSTStreamFrame e) =
    buildFramePayloadRSTStream einfo e
buildFramePayload einfo (SettingsFrame settings) =
    buildFramePayloadSettings einfo settings
buildFramePayload einfo (PushPromiseFrame sid hdr) =
    buildFramePayloadPushPromise einfo sid hdr
buildFramePayload einfo (PingFrame opaque) =
    buildFramePayloadPing einfo opaque
buildFramePayload einfo (GoAwayFrame sid e debug) =
    buildFramePayloadGoAway einfo sid e debug
buildFramePayload einfo (WindowUpdateFrame size) =
    buildFramePayloadWindowUpdate einfo size
buildFramePayload einfo (ContinuationFrame hdr) =
    buildFramePayloadContinuation einfo hdr
buildFramePayload einfo (UnknownFrame _ opaque) =
    buildFramePayloadUnknown einfo opaque

----------------------------------------------------------------

buildPadding :: EncodeInfo
             -> Builder
             -> Int -- ^ Payload length.
             -> (FrameHeader, Builder)
buildPadding EncodeInfo{ encodePadding = Nothing, ..} builder len =
    (header, builder)
  where
    header = FrameHeader len encodeFlags encodeStreamId
buildPadding EncodeInfo{ encodePadding = Just padding, ..} btarget targetLength =
    (header, builder)
  where
    header = FrameHeader len newflags encodeStreamId
    builder = (b1 :) . btarget . (padding :)
    b1 = BS.singleton $ fromIntegral paddingLength
    paddingLength = BS.length padding
    len = targetLength + paddingLength + 1
    newflags = setPadded encodeFlags

buildPriority :: Priority -> Builder
buildPriority Priority{..} = builder
  where
    builder = (priority :)
    estream
      | exclusive = setExclusive streamDependency
      | otherwise = streamDependency
    priority = unsafeCreate 5 $ \ptr -> do
        poke32 ptr $ fromIntegral estream
        poke8 ptr 4 $ fromIntegral $ weight - 1

----------------------------------------------------------------

buildFramePayloadData :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadData einfo body = buildPadding einfo builder len
  where
    builder = (body :)
    len = BS.length body

buildFramePayloadHeaders :: EncodeInfo -> Maybe Priority -> HeaderBlockFragment
                         -> (FrameHeader, Builder)
buildFramePayloadHeaders einfo Nothing hdr =
    buildPadding einfo builder len
  where
    builder = (hdr :)
    len = BS.length hdr
buildFramePayloadHeaders einfo (Just pri) hdr =
    buildPadding einfo' builder len
  where
    builder = buildPriority pri . (hdr :)
    len = BS.length hdr + 5
    einfo' = einfo { encodeFlags = setPriority (encodeFlags einfo) }

buildFramePayloadPriority :: EncodeInfo -> Priority -> (FrameHeader, Builder)
buildFramePayloadPriority EncodeInfo{..} p = (header, builder)
  where
    builder = buildPriority p
    header = FrameHeader 5 encodeFlags encodeStreamId

buildFramePayloadRSTStream :: EncodeInfo -> ErrorCodeId -> (FrameHeader, Builder)
buildFramePayloadRSTStream EncodeInfo{..} e = (header, builder)
  where
    builder = (b4 :)
    b4 = bytestring4 $ fromErrorCodeId e
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadSettings :: EncodeInfo -> SettingsList -> (FrameHeader, Builder)
buildFramePayloadSettings EncodeInfo{..} alist = (header, builder)
  where
    builder = (settings :)
    settings = unsafeCreate len $ \ptr -> go ptr alist
    go _ []          = return ()
    go p ((k,v):kvs) = do
        poke16 p $ fromSettingsKeyId k
        poke32 (p `plusPtr` 2) $ fromIntegral v
        go (p `plusPtr` 6) kvs
    len = length alist * 6
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadPushPromise :: EncodeInfo -> StreamId -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadPushPromise einfo sid hdr = buildPadding einfo builder len
  where
    builder = (b4 :) . (hdr :)
    b4 = bytestring4 $ fromIntegral sid
    len = 4 + BS.length hdr

buildFramePayloadPing :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadPing EncodeInfo{..} odata = (header, builder)
  where
    builder = (odata :)
    header = FrameHeader 8 encodeFlags encodeStreamId

buildFramePayloadGoAway :: EncodeInfo -> StreamId -> ErrorCodeId -> ByteString -> (FrameHeader, Builder)
buildFramePayloadGoAway EncodeInfo{..} sid e debug = (header, builder)
  where
    builder = (b8 :) . (debug :)
    len0 = 8
    b8 = unsafeCreate len0 $ \ptr -> do
        poke32 ptr $ fromIntegral sid
        poke32 (ptr `plusPtr` 4) $ fromErrorCodeId e
    len = len0 + BS.length debug
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadWindowUpdate :: EncodeInfo -> WindowSize -> (FrameHeader, Builder)
buildFramePayloadWindowUpdate EncodeInfo{..} size = (header, builder)
  where
    -- fixme: reserve bit
    builder = (b4 :)
    b4 = bytestring4 $ fromIntegral size
    header = FrameHeader 4 encodeFlags encodeStreamId

buildFramePayloadContinuation :: EncodeInfo -> HeaderBlockFragment -> (FrameHeader, Builder)
buildFramePayloadContinuation EncodeInfo{..} hdr = (header, builder)
  where
    builder = (hdr :)
    len = BS.length hdr
    header = FrameHeader len encodeFlags encodeStreamId

buildFramePayloadUnknown :: EncodeInfo -> ByteString -> (FrameHeader, Builder)
buildFramePayloadUnknown = buildFramePayloadData

----------------------------------------------------------------

poke8 :: Ptr Word8 -> Int -> Word8 -> IO ()
poke8 ptr n w = poke (ptr `plusPtr` n) w

poke16 :: Ptr Word8 -> Word16 -> IO ()
poke16 ptr i = do
    poke ptr w0
    poke8 ptr 1 w1
  where
    w0 = fromIntegral ((i `shiftR`  8) .&. 0xff)
    w1 = fromIntegral  (i              .&. 0xff)

poke24 :: Ptr Word8 -> Int -> IO ()
poke24 ptr i = do
    poke ptr w0
    poke8 ptr 1 w1
    poke8 ptr 2 w2
  where
    w0 = fromIntegral ((i `shiftR` 16) .&. 0xff)
    w1 = fromIntegral ((i `shiftR`  8) .&. 0xff)
    w2 = fromIntegral  (i              .&. 0xff)

poke32 :: Ptr Word8 -> Word32 -> IO ()
poke32 ptr i = do
    poke ptr w0
    poke8 ptr 1 w1
    poke8 ptr 2 w2
    poke8 ptr 3 w3
  where
    w0 = fromIntegral ((i `shiftR` 24) .&. 0xff)
    w1 = fromIntegral ((i `shiftR` 16) .&. 0xff)
    w2 = fromIntegral ((i `shiftR`  8) .&. 0xff)
    w3 = fromIntegral  (i              .&. 0xff)

bytestring4 :: Word32 -> ByteString
bytestring4 i = unsafeCreate 4 $ \ptr -> poke32 ptr i
