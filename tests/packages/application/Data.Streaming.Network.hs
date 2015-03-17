{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Streaming/Network.hs" #-}

































































{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Data.Streaming.Network
    ( -- * Types
      ServerSettings
    , ClientSettings
    , HostPreference
    , Message (..)
    , AppData
    , ServerSettingsUnix
    , ClientSettingsUnix
    , AppDataUnix
      -- ** Smart constructors
    , serverSettingsTCP
    , serverSettingsTCPSocket
    , clientSettingsTCP
    , serverSettingsUDP
    , clientSettingsUDP
    , serverSettingsUnix
    , clientSettingsUnix
    , message
      -- ** Classes
    , HasPort (..)
    , HasAfterBind (..)
    , HasReadWrite (..)
    , HasPath (..)
      -- ** Setters
    , setPort
    , setHost
    , setAddrFamily
    , setAfterBind
    , setNeedLocalAddr
    , setPath
      -- ** Getters
    , getPort
    , getHost
    , getAddrFamily
    , getAfterBind
    , getNeedLocalAddr
    , getPath
    , appRead
    , appWrite
    , appSockAddr
    , appLocalAddr
    , appCloseConnection
      -- * Functions
      -- ** General
    , bindPortGen
    , bindRandomPortGen
    , getSocketGen
    , getSocketFamilyGen
    , acceptSafe
    , unassignedPorts
    , getUnassignedPort
      -- ** TCP
    , bindPortTCP
    , bindRandomPortTCP
    , getSocketTCP
    , getSocketFamilyTCP
    , safeRecv
    , runTCPServer
    , runTCPClient
    , ConnectionHandle()
    , runTCPServerWithHandle
      -- ** UDP
    , bindPortUDP
    , bindRandomPortUDP
    , getSocketUDP
      -- ** Unix
    , bindPath
    , getSocketUnix
    , runUnixServer
    , runUnixClient
    ) where

import qualified Network.Socket as NS
import Data.Streaming.Network.Internal
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try, SomeException, throwIO, bracketOnError)
import Network.Socket (Socket, AddrInfo, SocketType)
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Char8 as S8
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import System.Directory (removeFile)
import Data.Functor.Constant (Constant (Constant), getConstant)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Data.Array.Unboxed ((!), UArray, listArray)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

-- | Attempt to connect to the given host/port/address family using given @SocketType@.
--
-- Since 0.1.3
getSocketFamilyGen :: SocketType -> String -> Int -> NS.Family -> IO (Socket, AddrInfo)
getSocketFamilyGen sockettype host' port' af = do
    let hints = NS.defaultHints {
                          NS.addrFlags = [NS.AI_ADDRCONFIG]
                        , NS.addrSocketType = sockettype
                        , NS.addrFamily = af
                        }
    (addr:_) <- NS.getAddrInfo (Just hints) (Just host') (Just $ show port')
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                      (NS.addrProtocol addr)
    return (sock, addr)

-- | Attempt to connect to the given host/port using given @SocketType@.
getSocketGen :: SocketType -> String -> Int -> IO (Socket, AddrInfo)
getSocketGen sockettype host port = getSocketFamilyGen sockettype host port NS.AF_UNSPEC

-- | Attempt to bind a listening @Socket@ on the given host/port using given
-- @SocketType@. If no host is given, will use the first address available.
bindPortGen :: SocketType -> Int -> HostPreference -> IO Socket
bindPortGen sockettype p s = do
    let hints = NS.defaultHints
            { NS.addrFlags = [ NS.AI_PASSIVE
                             , NS.AI_NUMERICSERV
                             , NS.AI_NUMERICHOST
                             ]
            , NS.addrSocketType = sockettype
            }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- NS.getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> NS.addrFamily x /= NS.AF_INET6) addrs
        addrs6 = filter (\x -> NS.addrFamily x == NS.AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4     -> addrs4 ++ addrs6
                HostIPv4Only -> addrs4
                HostIPv6     -> addrs6 ++ addrs4
                HostIPv6Only -> addrs6
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) =
                                      E.catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"

        sockOpts =
            case sockettype of
                NS.Datagram -> [(NS.ReuseAddr,1)]
                _           -> [(NS.NoDelay,1), (NS.ReuseAddr,1)]

        theBody addr =
          bracketOnError
          (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr))
          NS.sClose
          (\sock -> do
              mapM_ (\(opt,v) -> NS.setSocketOption sock opt v) sockOpts
              NS.bindSocket sock (NS.addrAddress addr)
              return sock
          )
    tryAddrs addrs'

-- | Bind to a random port number. Especially useful for writing network tests.
--
-- This will attempt 30 different port numbers before giving up and throwing an
-- exception.
--
-- Since 0.1.1
bindRandomPortGen :: SocketType -> HostPreference -> IO (Int, Socket)
bindRandomPortGen sockettype s =
    loop (30 :: Int)
  where
    loop cnt = do
        port <- getUnassignedPort
        esocket <- try $ bindPortGen sockettype port s
        case esocket :: Either IOException Socket of
            Left e
                | cnt <= 1 -> error $ concat
                    [ "Data.Streaming.Network.bindRandomPortGen: Could not get port. Last attempted: "
                    , show port
                    , ". Exception was: "
                    , show e
                    ]
                | otherwise -> do
                    skipUnassigned 50
                    loop $! cnt - 1
            Right socket -> return (port, socket)

-- | Top 10 Largest IANA unassigned port ranges with no unauthorized uses known
unassignedPortsList :: [Int]
unassignedPortsList = concat
    [ [43124..44320]
    , [28120..29166]
    , [45967..46997]
    , [28241..29117]
    , [40001..40840]
    , [29170..29998]
    , [38866..39680]
    , [43442..44122]
    , [41122..41793]
    , [35358..36000]
    ]

unassignedPorts :: UArray Int Int
unassignedPorts = listArray (unassignedPortsMin, unassignedPortsMax) unassignedPortsList

unassignedPortsMin, unassignedPortsMax :: Int
unassignedPortsMin = 0
unassignedPortsMax = length unassignedPortsList - 1

nextUnusedPort :: IORef Int
nextUnusedPort = unsafePerformIO
               $ randomRIO (unassignedPortsMin, unassignedPortsMax) >>= newIORef
{-# NOINLINE nextUnusedPort #-}

-- | Get a port from the IANA list of unassigned ports.
--
-- Internally, this function uses an @IORef@ to cycle through the list of ports
getUnassignedPort :: IO Int
getUnassignedPort = do
    port <- atomicModifyIORef nextUnusedPort go
    return $! port
  where
    go i
        | i > unassignedPortsMax = (succ unassignedPortsMin, unassignedPorts ! unassignedPortsMin)
        | otherwise = (succ i, unassignedPorts ! i)

-- | Skip ahead in the unassigned ports list by the given number
skipUnassigned :: Int -> IO ()
skipUnassigned i = do
    !() <- atomicModifyIORef nextUnusedPort $ \j ->
        let k = i + j `mod` unassignedPortsMax
         in k `seq` (k, ())
    return ()

-- | Attempt to connect to the given host/port.
getSocketUDP :: String -> Int -> IO (Socket, AddrInfo)
getSocketUDP = getSocketGen NS.Datagram

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
bindPortUDP :: Int -> HostPreference -> IO Socket
bindPortUDP = bindPortGen NS.Datagram

-- | Bind a random UDP port.
--
-- See 'bindRandomPortGen'
--
-- Since 0.1.1
bindRandomPortUDP :: HostPreference -> IO (Int, Socket)
bindRandomPortUDP = bindRandomPortGen NS.Datagram

-- | Attempt to connect to the given Unix domain socket path.
getSocketUnix :: FilePath -> IO Socket
getSocketUnix path = do
    sock <- NS.socket NS.AF_UNIX NS.Stream 0
    ee <- try' $ NS.connect sock (NS.SockAddrUnix path)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return sock
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

-- | Attempt to bind a listening Unix domain socket at the given path.
bindPath :: FilePath -> IO Socket
bindPath path = do
  sock <- bracketOnError
            (NS.socket NS.AF_UNIX NS.Stream 0)
            NS.sClose
            (\sock -> do
                removeFileSafe path  -- Cannot bind if the socket file exists.
                NS.bindSocket sock (NS.SockAddrUnix path)
                return sock)
  NS.listen sock (max 2048 NS.maxListenQueue)
  return sock

removeFileSafe :: FilePath -> IO ()
removeFileSafe path =
    removeFile path `E.catch` handleExists
  where
    handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | Smart constructor.
serverSettingsUnix
    :: FilePath -- ^ path to bind to
    -> ServerSettingsUnix
serverSettingsUnix path = ServerSettingsUnix
    { serverPath = path
    , serverAfterBindUnix = const $ return ()
    }

-- | Smart constructor.
clientSettingsUnix
    :: FilePath -- ^ path to connect to
    -> ClientSettingsUnix
clientSettingsUnix path = ClientSettingsUnix
    { clientPath = path
    }


safeRecv :: Socket -> Int -> IO ByteString
safeRecv = recv

-- | Smart constructor.
serverSettingsUDP
    :: Int -- ^ port to bind to
    -> HostPreference -- ^ host binding preferences
    -> ServerSettings
serverSettingsUDP = serverSettingsTCP

-- | Smart constructor.
serverSettingsTCP
    :: Int -- ^ port to bind to
    -> HostPreference -- ^ host binding preferences
    -> ServerSettings
serverSettingsTCP port host = ServerSettings
    { serverPort = port
    , serverHost = host
    , serverSocket = Nothing
    , serverAfterBind = const $ return ()
    , serverNeedLocalAddr = False
    }

-- | Create a server settings that uses an already available listening socket.
-- Any port and host modifications made to this value will be ignored.
--
-- Since 0.1.1
serverSettingsTCPSocket :: Socket -> ServerSettings
serverSettingsTCPSocket lsocket = ServerSettings
    { serverPort = 0
    , serverHost = HostAny
    , serverSocket = Just lsocket
    , serverAfterBind = const $ return ()
    , serverNeedLocalAddr = False
    }

-- | Smart constructor.
clientSettingsUDP
    :: Int -- ^ port to connect to
    -> ByteString -- ^ host to connect to
    -> ClientSettings
clientSettingsUDP = clientSettingsTCP

-- | Smart constructor.
clientSettingsTCP
    :: Int -- ^ port to connect to
    -> ByteString -- ^ host to connect to
    -> ClientSettings
clientSettingsTCP port host = ClientSettings
    { clientPort = port
    , clientHost = host
    , clientAddrFamily = NS.AF_UNSPEC
    }

-- | Attempt to connect to the given host/port/address family.
--
-- Since 0.1.3
getSocketFamilyTCP :: ByteString -> Int -> NS.Family -> IO (NS.Socket, NS.SockAddr)
getSocketFamilyTCP host' port' addrFamily = do
    (sock, addr) <- getSocketFamilyGen NS.Stream (S8.unpack host') port' addrFamily
    ee <- try' $ NS.connect sock (NS.addrAddress addr)
    case ee of
        Left e -> NS.sClose sock >> throwIO e
        Right () -> return (sock, NS.addrAddress addr)
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

-- | Attempt to connect to the given host/port.
getSocketTCP :: ByteString -> Int -> IO (NS.Socket, NS.SockAddr)
getSocketTCP host port = getSocketFamilyTCP host port NS.AF_UNSPEC

-- | Attempt to bind a listening @Socket@ on the given host/port. If no host is
-- given, will use the first address available.
-- 'maxListenQueue' is topically 128 which is too short for
-- high performance servers. So, we specify 'max 2048 maxListenQueue' to
-- the listen queue.
bindPortTCP :: Int -> HostPreference -> IO Socket
bindPortTCP p s = do
    sock <- bindPortGen NS.Stream p s
    NS.listen sock (max 2048 NS.maxListenQueue)
    return sock

-- | Bind a random TCP port.
--
-- See 'bindRandomPortGen'.
--
-- Since 0.1.1
bindRandomPortTCP :: HostPreference -> IO (Int, Socket)
bindRandomPortTCP s = do
    (port, sock) <- bindRandomPortGen NS.Stream s
    NS.listen sock (max 2048 NS.maxListenQueue)
    return (port, sock)

-- | Try to accept a connection, recovering automatically from exceptions.
--
-- As reported by Kazu against Warp, "resource exhausted (Too many open files)"
-- may be thrown by accept(). This function will catch that exception, wait a
-- second, and then try again.
acceptSafe :: Socket -> IO (Socket, NS.SockAddr)
acceptSafe socket =
    loop
  where
    loop =
        NS.accept socket `E.catch` \(_ :: IOException) -> do
            threadDelay 1000000
            loop

message :: ByteString -> NS.SockAddr -> Message
message = Message

class HasPort a where
    portLens :: Functor f => (Int -> f Int) -> a -> f a
instance HasPort ServerSettings where
    portLens f ss = fmap (\p -> ss { serverPort = p }) (f (serverPort ss))
instance HasPort ClientSettings where
    portLens f ss = fmap (\p -> ss { clientPort = p }) (f (clientPort ss))

getPort :: HasPort a => a -> Int
getPort = getConstant . portLens Constant

setPort :: HasPort a => Int -> a -> a
setPort p = runIdentity . portLens (const (Identity p))

setHost :: ByteString -> ClientSettings -> ClientSettings
setHost hp ss = ss { clientHost = hp }

getHost :: ClientSettings -> ByteString
getHost = clientHost

-- | Set the address family for the given settings.
--
-- Since 0.1.3
setAddrFamily :: NS.Family -> ClientSettings -> ClientSettings
setAddrFamily af cs = cs { clientAddrFamily = af }

-- | Get the address family for the given settings.
--
-- Since 0.1.3
getAddrFamily :: ClientSettings -> NS.Family
getAddrFamily = clientAddrFamily

class HasPath a where
    pathLens :: Functor f => (FilePath -> f FilePath) -> a -> f a
instance HasPath ServerSettingsUnix where
    pathLens f ss = fmap (\p -> ss { serverPath = p }) (f (serverPath ss))
instance HasPath ClientSettingsUnix where
    pathLens f ss = fmap (\p -> ss { clientPath = p }) (f (clientPath ss))

getPath :: HasPath a => a -> FilePath
getPath = getConstant . pathLens Constant

setPath :: HasPath a => FilePath -> a -> a
setPath p = runIdentity . pathLens (const (Identity p))

setNeedLocalAddr :: Bool -> ServerSettings -> ServerSettings
setNeedLocalAddr x y = y { serverNeedLocalAddr = x }

getNeedLocalAddr :: ServerSettings -> Bool
getNeedLocalAddr = serverNeedLocalAddr

class HasAfterBind a where
    afterBindLens :: Functor f => ((Socket -> IO ()) -> f (Socket -> IO ())) -> a -> f a
instance HasAfterBind ServerSettings where
    afterBindLens f ss = fmap (\p -> ss { serverAfterBind = p }) (f (serverAfterBind ss))
instance HasAfterBind ServerSettingsUnix where
    afterBindLens f ss = fmap (\p -> ss { serverAfterBindUnix = p }) (f (serverAfterBindUnix ss))

getAfterBind :: HasAfterBind a => a -> (Socket -> IO ())
getAfterBind = getConstant . afterBindLens Constant

setAfterBind :: HasAfterBind a => (Socket -> IO ()) -> a -> a
setAfterBind p = runIdentity . afterBindLens (const (Identity p))

type ConnectionHandle = Socket -> NS.SockAddr -> Maybe NS.SockAddr -> IO ()

runTCPServerWithHandle :: ServerSettings -> ConnectionHandle -> IO a
runTCPServerWithHandle (ServerSettings port host msocket afterBind needLocalAddr) handle =
    case msocket of
        Nothing -> E.bracket (bindPortTCP port host) NS.sClose inner
        Just lsocket -> inner lsocket
  where
    inner lsocket = afterBind lsocket >> forever (serve lsocket)
    serve lsocket = E.bracketOnError
        (acceptSafe lsocket)
        (\(socket, _) -> NS.sClose socket)
        $ \(socket, addr) -> do
            mlocal <- if needLocalAddr
                        then fmap Just $ NS.getSocketName socket
                        else return Nothing
            _ <- E.mask $ \restore -> forkIO
               $ restore (handle socket addr mlocal)
                    `E.finally` NS.sClose socket
            return ()



-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
runTCPServer :: ServerSettings -> (AppData -> IO ()) -> IO a
runTCPServer settings app = runTCPServerWithHandle settings app'
  where app' socket addr mlocal =
          let ad = AppData
                { appRead' = safeRecv socket 4096
                , appWrite' = sendAll socket
                , appSockAddr' = addr
                , appLocalAddr' = mlocal
                , appCloseConnection' = NS.sClose socket
                }
          in
            app ad

-- | Run an @Application@ by connecting to the specified server.
runTCPClient :: ClientSettings -> (AppData -> IO a) -> IO a
runTCPClient (ClientSettings port host addrFamily) app = E.bracket
    (getSocketFamilyTCP host port addrFamily)
    (NS.sClose . fst)
    (\(s, address) -> app AppData
        { appRead' = safeRecv s 4096
        , appWrite' = sendAll s
        , appSockAddr' = address
        , appLocalAddr' = Nothing
        , appCloseConnection' = NS.sClose s
        })

appLocalAddr :: AppData -> Maybe NS.SockAddr
appLocalAddr = appLocalAddr'

appSockAddr :: AppData -> NS.SockAddr
appSockAddr = appSockAddr'

-- | Close the underlying connection. One possible use case is simulating
-- connection failures in a test suite.
--
-- Since 0.1.6
appCloseConnection :: AppData -> IO ()
appCloseConnection = appCloseConnection'

class HasReadWrite a where
    readLens :: Functor f => (IO ByteString -> f (IO ByteString)) -> a -> f a
    writeLens :: Functor f => ((ByteString -> IO ()) -> f (ByteString -> IO ())) -> a -> f a
instance HasReadWrite AppData where
    readLens f a = fmap (\x -> a { appRead' = x }) (f (appRead' a))
    writeLens f a = fmap (\x -> a { appWrite' = x }) (f (appWrite' a))
instance HasReadWrite AppDataUnix where
    readLens f a = fmap (\x -> a { appReadUnix = x }) (f (appReadUnix a))
    writeLens f a = fmap (\x -> a { appWriteUnix = x }) (f (appWriteUnix a))

appRead :: HasReadWrite a => a -> IO ByteString
appRead = getConstant . readLens Constant

appWrite :: HasReadWrite a => a -> ByteString -> IO ()
appWrite = getConstant . writeLens Constant

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection.
runUnixServer :: ServerSettingsUnix -> (AppDataUnix -> IO ()) -> IO a
runUnixServer (ServerSettingsUnix path afterBind) app = E.bracket
    (bindPath path)
    NS.sClose
    (\socket -> do
        afterBind socket
        forever $ serve socket)
  where
    serve lsocket = E.bracketOnError
        (acceptSafe lsocket)
        (\(socket, _) -> NS.sClose socket)
        $ \(socket, _) -> do
            let ad = AppDataUnix
                    { appReadUnix = safeRecv socket 4096
                    , appWriteUnix = sendAll socket
                    }
            _ <- E.mask $ \restore -> forkIO
                $ restore (app ad)
                    `E.finally` NS.sClose socket
            return ()

-- | Run an @Application@ by connecting to the specified server.
runUnixClient :: ClientSettingsUnix -> (AppDataUnix -> IO a) -> IO a
runUnixClient (ClientSettingsUnix path) app = E.bracket
    (getSocketUnix path)
    NS.sClose
    (\sock -> app AppDataUnix
        { appReadUnix = safeRecv sock 4096
        , appWriteUnix = sendAll sock
        })
