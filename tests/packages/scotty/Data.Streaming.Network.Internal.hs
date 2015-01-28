{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/Network/Internal.hs" #-}

































































{-# LANGUAGE CPP #-}
module Data.Streaming.Network.Internal
    ( ServerSettings (..)
    , ClientSettings (..)
    , HostPreference (..)
    , Message (..)
    , AppData (..)
    , ServerSettingsUnix (..)
    , ClientSettingsUnix (..)
    , AppDataUnix (..)
    ) where

import Data.String (IsString (..))
import Data.ByteString (ByteString)
import Network.Socket (Socket, SockAddr, Family)

-- | Settings for a TCP server. It takes a port to listen on, and an optional
-- hostname to bind to.
data ServerSettings = ServerSettings
    { serverPort :: !Int
    , serverHost :: !HostPreference
    , serverSocket :: !(Maybe Socket) -- ^ listening socket
    , serverAfterBind :: !(Socket -> IO ())
    , serverNeedLocalAddr :: !Bool
    }

-- | Settings for a TCP client, specifying how to connect to the server.
data ClientSettings = ClientSettings
    { clientPort :: !Int
    , clientHost :: !ByteString
    , clientAddrFamily :: !Family
    }

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @!4@ means @HostIPv4Only@
--
-- * @*6@ means @HostIPv6@
--
-- * @!6@ means @HostIPv6Only@
--
-- Any other values is treated as a hostname. As an example, to bind to the
-- IPv4 local host only, use \"127.0.0.1\".
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv4Only
  | HostIPv6
  | HostIPv6Only
  | Host String
    deriving (Eq, Ord, Show, Read)

instance IsString HostPreference where
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "!4" = HostIPv4Only
    fromString "*6" = HostIPv6
    fromString "!6" = HostIPv6Only
    fromString s = Host s

-- | Settings for a Unix domain sockets server.
data ServerSettingsUnix = ServerSettingsUnix
    { serverPath :: !FilePath
    , serverAfterBindUnix :: !(Socket -> IO ())
    }

-- | Settings for a Unix domain sockets client.
data ClientSettingsUnix = ClientSettingsUnix
    { clientPath :: !FilePath
    }

-- | The data passed to a Unix domain sockets @Application@.
data AppDataUnix = AppDataUnix
    { appReadUnix :: !(IO ByteString)
    , appWriteUnix :: !(ByteString -> IO ())
    }

-- | Representation of a single UDP message
data Message = Message { msgData :: {-# UNPACK #-} !ByteString
                       , msgSender :: !SockAddr
                       }

-- | The data passed to an @Application@.
data AppData = AppData
    { appRead' :: !(IO ByteString)
    , appWrite' :: !(ByteString -> IO ())
    , appSockAddr' :: !SockAddr
    , appLocalAddr' :: !(Maybe SockAddr)
    , appCloseConnection' :: !(IO ())
    }
