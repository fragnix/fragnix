{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HTTP2.hs" #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Framing in HTTP\/2(<https://tools.ietf.org/html/rfc7540>).
module Network.HTTP2 (
  -- * Frame
    Frame(..)
  , FrameHeader(..)
  , FramePayload(..)
  , HeaderBlockFragment
  , Padding
  , isPaddingDefined
  -- * Encoding
  , encodeFrame
  , encodeFrameChunks
  , encodeFrameHeader
  , encodeFrameHeaderBuf
  , encodeFramePayload
  , EncodeInfo(..)
  , encodeInfo
  , module Network.HTTP2.Decode
  -- * Frame type ID
  , FrameTypeId(..)
  , framePayloadToFrameTypeId
  -- * Frame type
  , FrameType
  , fromFrameTypeId
  , toFrameTypeId
  -- * Priority
  , Priority(..)
  , Weight
  , defaultPriority
  , highestPriority
  -- * Stream identifier
  , StreamId
  , isControl
  , isRequest
  , isResponse
  -- * Stream identifier related
  , testExclusive
  , setExclusive
  , clearExclusive
  -- * Flags
  , FrameFlags
  , defaultFlags
  , testEndStream
  , testAck
  , testEndHeader
  , testPadded
  , testPriority
  , setEndStream
  , setAck
  , setEndHeader
  , setPadded
  , setPriority
  -- * SettingsList
  , SettingsList
  , SettingsKeyId(..)
  , SettingsValue
  , fromSettingsKeyId
  , toSettingsKeyId
  , checkSettingsList
  -- * Settings
  , Settings(..)
  , defaultSettings
  , updateSettings
  -- * Window
  , WindowSize
  , defaultInitialWindowSize
  , maxWindowSize
  , isWindowOverflow
  -- * Error code
  , ErrorCode
  , ErrorCodeId(..)
  , fromErrorCodeId
  , toErrorCodeId
  -- * Error
  , HTTP2Error(..)
  , errorCodeId
  -- * Predefined values
  , connectionPreface
  , connectionPrefaceLength
  , frameHeaderLength
  , maxPayloadLength
  , recommendedConcurrency
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.HTTP2.Decode
import Network.HTTP2.Encode
import Network.HTTP2.Types

-- | The preface of HTTP\/2.
--
-- >>> connectionPreface
-- "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

-- | Length of the preface.
--
-- >>> connectionPrefaceLength
-- 24
connectionPrefaceLength :: Int
connectionPrefaceLength = BS.length connectionPreface
