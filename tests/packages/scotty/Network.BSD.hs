{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/Network/BSD.hs" #-}
# 1 "dist/dist-sandbox-d76e0d17/build/Network/BSD.hs"
# 1 "<command-line>"
# 12 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 12 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1





























































































# 12 "<command-line>" 2
# 1 "dist/dist-sandbox-d76e0d17/build/Network/BSD.hs"
{-# LINE 1 "Network/BSD.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Network/BSD.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.BSD
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The "Network.BSD" module defines Haskell bindings to network
-- programming functionality provided by BSD Unix derivatives.
--
-----------------------------------------------------------------------------


{-# LINE 18 "Network/BSD.hsc" #-}

module Network.BSD
    (
    -- * Host names
      HostName
    , getHostName

    , HostEntry(..)
    , getHostByName
    , getHostByAddr
    , hostAddress


{-# LINE 31 "Network/BSD.hsc" #-}
    , getHostEntries

    -- ** Low level functionality
    , setHostEntry
    , getHostEntry
    , endHostEntry

{-# LINE 38 "Network/BSD.hsc" #-}

    -- * Service names
    , ServiceEntry(..)
    , ServiceName
    , getServiceByName
    , getServiceByPort
    , getServicePortNumber


{-# LINE 47 "Network/BSD.hsc" #-}
    , getServiceEntries

    -- ** Low level functionality
    , getServiceEntry
    , setServiceEntry
    , endServiceEntry

{-# LINE 54 "Network/BSD.hsc" #-}

    -- * Protocol names
    , ProtocolName
    , ProtocolNumber
    , ProtocolEntry(..)
    , getProtocolByName
    , getProtocolByNumber
    , getProtocolNumber
    , defaultProtocol


{-# LINE 65 "Network/BSD.hsc" #-}
    , getProtocolEntries
    -- ** Low level functionality
    , setProtocolEntry
    , getProtocolEntry
    , endProtocolEntry

{-# LINE 71 "Network/BSD.hsc" #-}

    -- * Port numbers
    , PortNumber

    -- * Network names
    , NetworkName
    , NetworkAddr
    , NetworkEntry(..)


{-# LINE 81 "Network/BSD.hsc" #-}
    , getNetworkByName
    , getNetworkByAddr
    , getNetworkEntries
    -- ** Low level functionality
    , setNetworkEntry
    , getNetworkEntry
    , endNetworkEntry

{-# LINE 89 "Network/BSD.hsc" #-}
    ) where

import Network.Socket

import Control.Concurrent (MVar, newMVar, withMVar)
import qualified Control.Exception as E
import Foreign.C.String (CString, peekCString, withCString)

{-# LINE 99 "Network/BSD.hsc" #-}
import Foreign.C.Types ( CInt(..), CULong(..), CSize(..) )
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (allocaArray0, peekArray0)
import Foreign.Marshal.Utils (with, fromBool)
import Data.Typeable
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.IO.Unsafe (unsafePerformIO)

import GHC.IO.Exception

import Control.Monad (liftM)

import Network.Socket.Internal (throwSocketErrorIfMinus1_)

-- ---------------------------------------------------------------------------
-- Basic Types

type ProtocolName = String

-- ---------------------------------------------------------------------------
-- Service Database Access

-- Calling getServiceByName for a given service and protocol returns
-- the systems service entry.  This should be used to find the port
-- numbers for standard protocols such as SMTP and FTP.  The remaining
-- three functions should be used for browsing the service database
-- sequentially.

-- Calling setServiceEntry with True indicates that the service
-- database should be left open between calls to getServiceEntry.  To
-- close the database a call to endServiceEntry is required.  This
-- database file is usually stored in the file /etc/services.

data ServiceEntry  =
  ServiceEntry  {
     serviceName     :: ServiceName,    -- Official Name
     serviceAliases  :: [ServiceName],  -- aliases
     servicePort     :: PortNumber,     -- Port Number  ( network byte order )
     serviceProtocol :: ProtocolName    -- Protocol
  } deriving (Show, Typeable)

instance Storable ServiceEntry where
   sizeOf    _ = 32
{-# LINE 143 "Network/BSD.hsc" #-}
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        s_name    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p >>= peekCString
{-# LINE 147 "Network/BSD.hsc" #-}
        s_aliases <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 148 "Network/BSD.hsc" #-}
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString
        s_port    <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 151 "Network/BSD.hsc" #-}
        s_proto   <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p >>= peekCString
{-# LINE 152 "Network/BSD.hsc" #-}
        return (ServiceEntry {
                        serviceName     = s_name,
                        serviceAliases  = s_aliases,

{-# LINE 158 "Network/BSD.hsc" #-}
                           -- s_port is already in network byte order, but it
                           -- might be the wrong size.
                        servicePort     = PortNum (fromIntegral (s_port :: CInt)),

{-# LINE 162 "Network/BSD.hsc" #-}
                        serviceProtocol = s_proto
                })

   poke _p = error "Storable.poke(BSD.ServiceEntry) not implemented"


-- | Get service by name.
getServiceByName :: ServiceName         -- Service Name
                 -> ProtocolName        -- Protocol Name
                 -> IO ServiceEntry     -- Service Entry
getServiceByName name proto = withLock $ do
 withCString name  $ \ cstr_name  -> do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "getServiceByName" "no such service entry"
   $ (trySysCall (c_getservbyname cstr_name cstr_proto))
 >>= peek

foreign import ccall unsafe "getservbyname"
  c_getservbyname :: CString -> CString -> IO (Ptr ServiceEntry)

-- | Get the service given a 'PortNumber' and 'ProtocolName'.
getServiceByPort :: PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort (PortNum port) proto = withLock $ do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "getServiceByPort" "no such service entry"
   $ (trySysCall (c_getservbyport (fromIntegral port) cstr_proto))
 >>= peek

foreign import ccall unsafe "getservbyport"
  c_getservbyport :: CInt -> CString -> IO (Ptr ServiceEntry)

-- | Get the 'PortNumber' corresponding to the 'ServiceName'.
getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port


{-# LINE 200 "Network/BSD.hsc" #-}
getServiceEntry :: IO ServiceEntry
getServiceEntry = withLock $ do
 throwNoSuchThingIfNull "getServiceEntry" "no such service entry"
   $ trySysCall c_getservent
 >>= peek

foreign import ccall unsafe "getservent" c_getservent :: IO (Ptr ServiceEntry)

setServiceEntry :: Bool -> IO ()
setServiceEntry flg = withLock $ trySysCall $ c_setservent (fromBool flg)

foreign import ccall unsafe  "setservent" c_setservent :: CInt -> IO ()

endServiceEntry :: IO ()
endServiceEntry = withLock $ trySysCall $ c_endservent

foreign import ccall unsafe  "endservent" c_endservent :: IO ()

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries stayOpen = do
  setServiceEntry stayOpen
  getEntries (getServiceEntry) (endServiceEntry)

{-# LINE 223 "Network/BSD.hsc" #-}

-- ---------------------------------------------------------------------------
-- Protocol Entries

-- The following relate directly to the corresponding UNIX C
-- calls for returning the protocol entries. The protocol entry is
-- represented by the Haskell type ProtocolEntry.

-- As for setServiceEntry above, calling setProtocolEntry.
-- determines whether or not the protocol database file, usually
-- @/etc/protocols@, is to be kept open between calls of
-- getProtocolEntry. Similarly,

data ProtocolEntry =
  ProtocolEntry  {
     protoName    :: ProtocolName,      -- Official Name
     protoAliases :: [ProtocolName],    -- aliases
     protoNumber  :: ProtocolNumber     -- Protocol Number
  } deriving (Read, Show, Typeable)

instance Storable ProtocolEntry where
   sizeOf    _ = 24
{-# LINE 245 "Network/BSD.hsc" #-}
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        p_name    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p >>= peekCString
{-# LINE 249 "Network/BSD.hsc" #-}
        p_aliases <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 250 "Network/BSD.hsc" #-}
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString

{-# LINE 259 "Network/BSD.hsc" #-}
        p_proto        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 260 "Network/BSD.hsc" #-}

{-# LINE 261 "Network/BSD.hsc" #-}
        return (ProtocolEntry {
                        protoName    = p_name,
                        protoAliases = p_aliases,
                        protoNumber  = p_proto
                })

   poke _p = error "Storable.poke(BSD.ProtocolEntry) not implemented"

getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = withLock $ do
 withCString name $ \ name_cstr -> do
 throwNoSuchThingIfNull "getProtocolByName" ("no such protocol name: " ++ name)
   $ (trySysCall.c_getprotobyname) name_cstr
 >>= peek

foreign import  ccall unsafe  "getprotobyname"
   c_getprotobyname :: CString -> IO (Ptr ProtocolEntry)


getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = withLock $ do
 throwNoSuchThingIfNull "getProtocolByNumber" ("no such protocol number: " ++ show num)
   $ (trySysCall.c_getprotobynumber) (fromIntegral num)
 >>= peek

foreign import ccall unsafe  "getprotobynumber"
   c_getprotobynumber :: CInt -> IO (Ptr ProtocolEntry)


getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num


{-# LINE 296 "Network/BSD.hsc" #-}
getProtocolEntry :: IO ProtocolEntry    -- Next Protocol Entry from DB
getProtocolEntry = withLock $ do
 ent <- throwNoSuchThingIfNull "getProtocolEntry" "no such protocol entry"
                $ trySysCall c_getprotoent
 peek ent

foreign import ccall unsafe  "getprotoent" c_getprotoent :: IO (Ptr ProtocolEntry)

setProtocolEntry :: Bool -> IO ()       -- Keep DB Open ?
setProtocolEntry flg = withLock $ trySysCall $ c_setprotoent (fromBool flg)

foreign import ccall unsafe "setprotoent" c_setprotoent :: CInt -> IO ()

endProtocolEntry :: IO ()
endProtocolEntry = withLock $ trySysCall $ c_endprotoent

foreign import ccall unsafe "endprotoent" c_endprotoent :: IO ()

getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = withLock $ do
  setProtocolEntry stayOpen
  getEntries (getProtocolEntry) (endProtocolEntry)

{-# LINE 319 "Network/BSD.hsc" #-}

-- ---------------------------------------------------------------------------
-- Host lookups

data HostEntry =
  HostEntry  {
     hostName      :: HostName,         -- Official Name
     hostAliases   :: [HostName],       -- aliases
     hostFamily    :: Family,           -- Host Type (currently AF_INET)
     hostAddresses :: [HostAddress]     -- Set of Network Addresses  (in network byte order)
  } deriving (Read, Show, Typeable)

instance Storable HostEntry where
   sizeOf    _ = 32
{-# LINE 333 "Network/BSD.hsc" #-}
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        h_name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p >>= peekCString
{-# LINE 337 "Network/BSD.hsc" #-}
        h_aliases    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 338 "Network/BSD.hsc" #-}
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        h_addrtype   <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 341 "Network/BSD.hsc" #-}
        -- h_length       <- (#peek struct hostent, h_length) p
        h_addr_list  <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 343 "Network/BSD.hsc" #-}
                                >>= peekArray0 nullPtr
                                >>= mapM peek
        return (HostEntry {
                        hostName       = h_name,
                        hostAliases    = h_aliases,

{-# LINE 351 "Network/BSD.hsc" #-}
                        hostFamily     = unpackFamily h_addrtype,

{-# LINE 353 "Network/BSD.hsc" #-}
                        hostAddresses  = h_addr_list
                })

   poke _p = error "Storable.poke(BSD.ServiceEntry) not implemented"


-- convenience function:
hostAddress :: HostEntry -> HostAddress
hostAddress (HostEntry nm _ _ ls) =
 case ls of
   []    -> error ("BSD.hostAddress: empty network address list for " ++ nm)
   (x:_) -> x

-- getHostByName must use the same lock as the *hostent functions
-- may cause problems if called concurrently.

-- | Resolve a 'HostName' to IPv4 address.
getHostByName :: HostName -> IO HostEntry
getHostByName name = withLock $ do
  withCString name $ \ name_cstr -> do
   ent <- throwNoSuchThingIfNull "getHostByName" "no such host entry"
                $ trySysCall $ c_gethostbyname name_cstr
   peek ent

foreign import ccall safe "gethostbyname"
   c_gethostbyname :: CString -> IO (Ptr HostEntry)


-- The locking of gethostbyaddr is similar to gethostbyname.
-- | Get a 'HostEntry' corresponding to the given address and family.
-- Note that only IPv4 is currently supported.
getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = do
 with addr $ \ ptr_addr -> withLock $ do
 throwNoSuchThingIfNull         "getHostByAddr" "no such host entry"
   $ trySysCall $ c_gethostbyaddr ptr_addr (fromIntegral (sizeOf addr)) (packFamily family)
 >>= peek

foreign import ccall safe "gethostbyaddr"
   c_gethostbyaddr :: Ptr HostAddress -> CInt -> CInt -> IO (Ptr HostEntry)


{-# LINE 395 "Network/BSD.hsc" #-}
getHostEntry :: IO HostEntry
getHostEntry = withLock $ do
 throwNoSuchThingIfNull         "getHostEntry" "unable to retrieve host entry"
   $ trySysCall $ c_gethostent
 >>= peek

foreign import ccall unsafe "gethostent" c_gethostent :: IO (Ptr HostEntry)

setHostEntry :: Bool -> IO ()
setHostEntry flg = withLock $ trySysCall $ c_sethostent (fromBool flg)

foreign import ccall unsafe "sethostent" c_sethostent :: CInt -> IO ()

endHostEntry :: IO ()
endHostEntry = withLock $ c_endhostent

foreign import ccall unsafe "endhostent" c_endhostent :: IO ()

getHostEntries :: Bool -> IO [HostEntry]
getHostEntries stayOpen = do
  setHostEntry stayOpen
  getEntries (getHostEntry) (endHostEntry)

{-# LINE 418 "Network/BSD.hsc" #-}

-- ---------------------------------------------------------------------------
-- Accessing network information

-- Same set of access functions as for accessing host,protocol and
-- service system info, this time for the types of networks supported.

-- network addresses are represented in host byte order.
type NetworkAddr = CULong

type NetworkName = String

data NetworkEntry =
  NetworkEntry {
     networkName        :: NetworkName,   -- official name
     networkAliases     :: [NetworkName], -- aliases
     networkFamily      :: Family,         -- type
     networkAddress     :: NetworkAddr
   } deriving (Read, Show, Typeable)

instance Storable NetworkEntry where
   sizeOf    _ = 32
{-# LINE 440 "Network/BSD.hsc" #-}
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        n_name         <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p >>= peekCString
{-# LINE 444 "Network/BSD.hsc" #-}
        n_aliases      <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 445 "Network/BSD.hsc" #-}
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        n_addrtype     <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 448 "Network/BSD.hsc" #-}
        n_net          <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p
{-# LINE 449 "Network/BSD.hsc" #-}
        return (NetworkEntry {
                        networkName      = n_name,
                        networkAliases   = n_aliases,
                        networkFamily    = unpackFamily (fromIntegral
                                                        (n_addrtype :: CInt)),
                        networkAddress   = n_net
                })

   poke _p = error "Storable.poke(BSD.NetEntry) not implemented"



{-# LINE 461 "Network/BSD.hsc" #-}
getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = withLock $ do
 withCString name $ \ name_cstr -> do
  throwNoSuchThingIfNull "getNetworkByName" "no such network entry"
    $ trySysCall $ c_getnetbyname name_cstr
  >>= peek

foreign import ccall unsafe "getnetbyname"
   c_getnetbyname  :: CString -> IO (Ptr NetworkEntry)

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr family = withLock $ do
 throwNoSuchThingIfNull "getNetworkByAddr" "no such network entry"
   $ trySysCall $ c_getnetbyaddr addr (packFamily family)
 >>= peek

foreign import ccall unsafe "getnetbyaddr"
   c_getnetbyaddr  :: NetworkAddr -> CInt -> IO (Ptr NetworkEntry)

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = withLock $ do
 throwNoSuchThingIfNull "getNetworkEntry" "no more network entries"
          $ trySysCall $ c_getnetent
 >>= peek

foreign import ccall unsafe "getnetent" c_getnetent :: IO (Ptr NetworkEntry)

-- | Open the network name database. The parameter specifies
-- whether a connection is maintained open between various
-- networkEntry calls
setNetworkEntry :: Bool -> IO ()
setNetworkEntry flg = withLock $ trySysCall $ c_setnetent (fromBool flg)

foreign import ccall unsafe "setnetent" c_setnetent :: CInt -> IO ()

-- | Close the connection to the network name database.
endNetworkEntry :: IO ()
endNetworkEntry = withLock $ trySysCall $ c_endnetent

foreign import ccall unsafe "endnetent" c_endnetent :: IO ()

-- | Get the list of network entries.
getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = do
  setNetworkEntry stayOpen
  getEntries (getNetworkEntry) (endNetworkEntry)

{-# LINE 508 "Network/BSD.hsc" #-}

-- Mutex for name service lockdown

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

withLock :: IO a -> IO a
withLock act = withMVar lock (\_ -> act)

-- ---------------------------------------------------------------------------
-- Miscellaneous Functions

-- | Calling getHostName returns the standard host name for the current
-- processor, as set at boot time.

getHostName :: IO HostName
getHostName = do
  let size = 256
  allocaArray0 size $ \ cstr -> do
    throwSocketErrorIfMinus1_ "getHostName" $ c_gethostname cstr (fromIntegral size)
    peekCString cstr

foreign import ccall unsafe "gethostname"
   c_gethostname :: CString -> CSize -> IO CInt

-- Helper function used by the exported functions that provides a
-- Haskellised view of the enumerator functions:

getEntries :: IO a  -- read
           -> IO () -- at end
           -> IO [a]
getEntries getOne atEnd = loop
  where
    loop = do
      vv <- E.catch (liftM Just getOne)
            (\ e -> let _types = e :: IOException in return Nothing)
      case vv of
        Nothing -> return []
        Just v  -> loop >>= \ vs -> atEnd >> return (v:vs)


-- ---------------------------------------------------------------------------
-- Winsock only:
--   The BSD API networking calls made locally return NULL upon failure.
--   That failure may very well be due to WinSock not being initialised,
--   so if NULL is seen try init'ing and repeat the call.

{-# LINE 556 "Network/BSD.hsc" #-}
trySysCall :: IO a -> IO a
trySysCall act = act

{-# LINE 566 "Network/BSD.hsc" #-}

throwNoSuchThingIfNull :: String -> String -> IO (Ptr a) -> IO (Ptr a)
throwNoSuchThingIfNull loc desc act = do
  ptr <- act
  if (ptr == nullPtr)
   then ioError (ioeSetErrorString (mkIOError NoSuchThing loc Nothing Nothing) desc)
   else return ptr
