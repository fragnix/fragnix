{-# LANGUAGE Haskell98, CPP, DeriveDataTypeable, ForeignFunctionInterface, TypeSynonymInstances #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/Network/Socket/Types.hs" #-}




















































{-# LINE 1 "Network/Socket/Types.hsc" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LINE 2 "Network/Socket/Types.hsc" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}


{-# LINE 6 "Network/Socket/Types.hsc" #-}

module Network.Socket.Types
    (
    -- * Socket
      Socket(..)
    , sockFd
    , sockFamily
    , sockType
    , sockProtocol
    , sockStatus
    , SocketStatus(..)

    -- * Socket types
    , SocketType(..)
    , isSupportedSocketType
    , packSocketType
    , packSocketType'
    , packSocketTypeOrThrow
    , unpackSocketType
    , unpackSocketType'

    -- * Family
    , Family(..)
    , isSupportedFamily
    , packFamily
    , unpackFamily

    -- * Socket addresses
    , SockAddr(..)
    , isSupportedSockAddr
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress

{-# LINE 40 "Network/Socket/Types.hsc" #-}
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID

{-# LINE 46 "Network/Socket/Types.hsc" #-}
    , peekSockAddr
    , pokeSockAddr
    , sizeOfSockAddr
    , sizeOfSockAddrByFamily
    , withSockAddr
    , withNewSockAddr

    -- * Unsorted
    , ProtocolNumber
    , PortNumber(..)

    -- * Low-level helpers
    , zeroMemory
    ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Maybe
import Data.Ratio
import Data.Typeable
import Data.Word
import Data.Int
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

-- | Represents a socket.  The fields are, respectively:
--
--   * File descriptor
--   * Socket family
--   * Socket type
--   * Protocol number
--   * Status flag
--
--   If you are calling the 'MkSocket' constructor directly you should ensure
--   you have called 'Network.withSocketsDo' and that the file descriptor is
--   in non-blocking mode. See 'Network.Socket.setNonBlockIfNeeded'.
data Socket
  = MkSocket
            CInt                 -- File Descriptor
            Family
            SocketType
            ProtocolNumber       -- Protocol Number
            (MVar SocketStatus)  -- Status Flag
  deriving Typeable

sockFd :: Socket -> CInt
sockFd       (MkSocket n _ _ _ _) = n

sockFamily :: Socket -> Family
sockFamily   (MkSocket _ f _ _ _) = f

sockType :: Socket -> SocketType
sockType     (MkSocket _ _ t _ _) = t

sockProtocol :: Socket -> ProtocolNumber
sockProtocol (MkSocket _ _ _ p _) = p

sockStatus :: Socket -> MVar SocketStatus
sockStatus   (MkSocket _ _ _ _ s) = s

instance Eq Socket where
  (MkSocket _ _ _ _ m1) == (MkSocket _ _ _ _ m2) = m1 == m2

instance Show Socket where
  showsPrec _n (MkSocket fd _ _ _ _) =
        showString "<socket: " . shows fd . showString ">"

type ProtocolNumber = CInt

-- | The status of the socket as /determined by this library/, not
-- necessarily reflecting the state of the connection itself.
--
-- For example, the 'Closed' status is applied when the 'close'
-- function is called.
data SocketStatus
  -- Returned Status    Function called
  = NotConnected        -- ^ Newly created, unconnected socket
  | Bound               -- ^ Bound, via 'bind'
  | Listening           -- ^ Listening, via 'listen'
  | Connected           -- ^ Connected or accepted, via 'connect' or 'accept'
  | ConvertedToHandle   -- ^ Is now a 'Handle' (via 'socketToHandle'), don't touch
  | Closed              -- ^ Closed was closed by 'close'
    deriving (Eq, Show, Typeable)

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

-- | Socket Types.
--
-- The existence of a constructor does not necessarily imply that that
-- socket type is supported on your system: see 'isSupportedSocketType'.
data SocketType
        = NoSocketType -- ^ 0, used in getAddrInfo hints, for example
        | Stream -- ^ SOCK_STREAM
        | Datagram -- ^ SOCK_DGRAM
        | Raw -- ^ SOCK_RAW
        | RDM -- ^ SOCK_RDM
        | SeqPacket -- ^ SOCK_SEQPACKET
        deriving (Eq, Ord, Read, Show, Typeable)

-- | Does the SOCK_ constant corresponding to the given SocketType exist on
-- this system?
isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType = isJust . packSocketType'

-- | Find the SOCK_ constant corresponding to the SocketType value.
packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType -> Just 0

{-# LINE 176 "Network/Socket/Types.hsc" #-}
    Just Stream -> Just 1
{-# LINE 177 "Network/Socket/Types.hsc" #-}

{-# LINE 178 "Network/Socket/Types.hsc" #-}

{-# LINE 179 "Network/Socket/Types.hsc" #-}
    Just Datagram -> Just 2
{-# LINE 180 "Network/Socket/Types.hsc" #-}

{-# LINE 181 "Network/Socket/Types.hsc" #-}

{-# LINE 182 "Network/Socket/Types.hsc" #-}
    Just Raw -> Just 3
{-# LINE 183 "Network/Socket/Types.hsc" #-}

{-# LINE 184 "Network/Socket/Types.hsc" #-}

{-# LINE 185 "Network/Socket/Types.hsc" #-}
    Just RDM -> Just 4
{-# LINE 186 "Network/Socket/Types.hsc" #-}

{-# LINE 187 "Network/Socket/Types.hsc" #-}

{-# LINE 188 "Network/Socket/Types.hsc" #-}
    Just SeqPacket -> Just 5
{-# LINE 189 "Network/Socket/Types.hsc" #-}

{-# LINE 190 "Network/Socket/Types.hsc" #-}
    _ -> Nothing

packSocketType :: SocketType -> CInt
packSocketType stype = fromMaybe (error errMsg) (packSocketType' stype)
  where
    errMsg = concat ["Network.Socket.packSocketType: ",
                     "socket type ", show stype, " unsupported on this system"]

-- | Try packSocketType' on the SocketType, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
packSocketTypeOrThrow :: String -> SocketType -> IO CInt
packSocketTypeOrThrow caller stype = maybe err return (packSocketType' stype)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show stype, " unsupported on this system"]


unpackSocketType:: CInt -> Maybe SocketType
unpackSocketType t = case t of
        0 -> Just NoSocketType

{-# LINE 211 "Network/Socket/Types.hsc" #-}
        (1) -> Just Stream
{-# LINE 212 "Network/Socket/Types.hsc" #-}

{-# LINE 213 "Network/Socket/Types.hsc" #-}

{-# LINE 214 "Network/Socket/Types.hsc" #-}
        (2) -> Just Datagram
{-# LINE 215 "Network/Socket/Types.hsc" #-}

{-# LINE 216 "Network/Socket/Types.hsc" #-}

{-# LINE 217 "Network/Socket/Types.hsc" #-}
        (3) -> Just Raw
{-# LINE 218 "Network/Socket/Types.hsc" #-}

{-# LINE 219 "Network/Socket/Types.hsc" #-}

{-# LINE 220 "Network/Socket/Types.hsc" #-}
        (4) -> Just RDM
{-# LINE 221 "Network/Socket/Types.hsc" #-}

{-# LINE 222 "Network/Socket/Types.hsc" #-}

{-# LINE 223 "Network/Socket/Types.hsc" #-}
        (5) -> Just SeqPacket
{-# LINE 224 "Network/Socket/Types.hsc" #-}

{-# LINE 225 "Network/Socket/Types.hsc" #-}
        _ -> Nothing

-- | Try unpackSocketType on the CInt, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
unpackSocketType' :: String -> CInt -> IO SocketType
unpackSocketType' caller ty = maybe err return (unpackSocketType ty)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show ty, " unsupported on this system"]

------------------------------------------------------------------------
-- Protocol Families.

-- | Address families.
--
-- A constructor being present here does not mean it is supported by the
-- operating system: see 'isSupportedFamily'.
data Family
    = AF_UNSPEC           -- unspecified
    | AF_UNIX             -- local to host (pipes, portals
    | AF_INET             -- internetwork: UDP, TCP, etc
    | AF_INET6            -- Internet Protocol version 6
    | AF_IMPLINK          -- arpanet imp addresses
    | AF_PUP              -- pup protocols: e.g. BSP
    | AF_CHAOS            -- mit CHAOS protocols
    | AF_NS               -- XEROX NS protocols
    | AF_NBS              -- nbs protocols
    | AF_ECMA             -- european computer manufacturers
    | AF_DATAKIT          -- datakit protocols
    | AF_CCITT            -- CCITT protocols, X.25 etc
    | AF_SNA              -- IBM SNA
    | AF_DECnet           -- DECnet
    | AF_DLI              -- Direct data link interface
    | AF_LAT              -- LAT
    | AF_HYLINK           -- NSC Hyperchannel
    | AF_APPLETALK        -- Apple Talk
    | AF_ROUTE            -- Internal Routing Protocol
    | AF_NETBIOS          -- NetBios-style addresses
    | AF_NIT              -- Network Interface Tap
    | AF_802              -- IEEE 802.2, also ISO 8802
    | AF_ISO              -- ISO protocols
    | AF_OSI              -- umbrella of all families used by OSI
    | AF_NETMAN           -- DNA Network Management
    | AF_X25              -- CCITT X.25
    | AF_AX25
    | AF_OSINET           -- AFI
    | AF_GOSSIP           -- US Government OSI
    | AF_IPX              -- Novell Internet Protocol
    | Pseudo_AF_XTP       -- eXpress Transfer Protocol (no AF)
    | AF_CTF              -- Common Trace Facility
    | AF_WAN              -- Wide Area Network protocols
    | AF_SDL              -- SGI Data Link for DLPI
    | AF_NETWARE
    | AF_NDD
    | AF_INTF             -- Debugging use only
    | AF_COIP             -- connection-oriented IP, aka ST II
    | AF_CNT              -- Computer Network Technology
    | Pseudo_AF_RTIP      -- Help Identify RTIP packets
    | Pseudo_AF_PIP       -- Help Identify PIP packets
    | AF_SIP              -- Simple Internet Protocol
    | AF_ISDN             -- Integrated Services Digital Network
    | Pseudo_AF_KEY       -- Internal key-management function
    | AF_NATM             -- native ATM access
    | AF_ARP              -- (rev.) addr. res. prot. (RFC 826)
    | Pseudo_AF_HDRCMPLT  -- Used by BPF to not rewrite hdrs in iface output
    | AF_ENCAP
    | AF_LINK             -- Link layer interface
    | AF_RAW              -- Link layer interface
    | AF_RIF              -- raw interface
    | AF_NETROM           -- Amateur radio NetROM
    | AF_BRIDGE           -- multiprotocol bridge
    | AF_ATMPVC           -- ATM PVCs
    | AF_ROSE             -- Amateur Radio X.25 PLP
    | AF_NETBEUI          -- 802.2LLC
    | AF_SECURITY         -- Security callback pseudo AF
    | AF_PACKET           -- Packet family
    | AF_ASH              -- Ash
    | AF_ECONET           -- Acorn Econet
    | AF_ATMSVC           -- ATM SVCs
    | AF_IRDA             -- IRDA sockets
    | AF_PPPOX            -- PPPoX sockets
    | AF_WANPIPE          -- Wanpipe API sockets
    | AF_BLUETOOTH        -- bluetooth sockets
    | AF_CAN              -- Controller Area Network
      deriving (Eq, Ord, Read, Show)

packFamily :: Family -> CInt
packFamily f = case packFamily' f of
    Just fam -> fam
    Nothing -> error $
               "Network.Socket.packFamily: unsupported address family: " ++
               show f

-- | Does the AF_ constant corresponding to the given family exist on this
-- system?
isSupportedFamily :: Family -> Bool
isSupportedFamily = isJust . packFamily'

packFamily' :: Family -> Maybe CInt
packFamily' f = case Just f of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just AF_UNSPEC -> Just 0
{-# LINE 328 "Network/Socket/Types.hsc" #-}

{-# LINE 329 "Network/Socket/Types.hsc" #-}
    Just AF_UNIX -> Just 1
{-# LINE 330 "Network/Socket/Types.hsc" #-}

{-# LINE 331 "Network/Socket/Types.hsc" #-}

{-# LINE 332 "Network/Socket/Types.hsc" #-}
    Just AF_INET -> Just 2
{-# LINE 333 "Network/Socket/Types.hsc" #-}

{-# LINE 334 "Network/Socket/Types.hsc" #-}

{-# LINE 335 "Network/Socket/Types.hsc" #-}
    Just AF_INET6 -> Just 10
{-# LINE 336 "Network/Socket/Types.hsc" #-}

{-# LINE 337 "Network/Socket/Types.hsc" #-}

{-# LINE 340 "Network/Socket/Types.hsc" #-}

{-# LINE 343 "Network/Socket/Types.hsc" #-}

{-# LINE 346 "Network/Socket/Types.hsc" #-}

{-# LINE 349 "Network/Socket/Types.hsc" #-}

{-# LINE 352 "Network/Socket/Types.hsc" #-}

{-# LINE 355 "Network/Socket/Types.hsc" #-}

{-# LINE 358 "Network/Socket/Types.hsc" #-}

{-# LINE 361 "Network/Socket/Types.hsc" #-}

{-# LINE 362 "Network/Socket/Types.hsc" #-}
    Just AF_SNA -> Just 22
{-# LINE 363 "Network/Socket/Types.hsc" #-}

{-# LINE 364 "Network/Socket/Types.hsc" #-}

{-# LINE 365 "Network/Socket/Types.hsc" #-}
    Just AF_DECnet -> Just 12
{-# LINE 366 "Network/Socket/Types.hsc" #-}

{-# LINE 367 "Network/Socket/Types.hsc" #-}

{-# LINE 370 "Network/Socket/Types.hsc" #-}

{-# LINE 373 "Network/Socket/Types.hsc" #-}

{-# LINE 376 "Network/Socket/Types.hsc" #-}

{-# LINE 377 "Network/Socket/Types.hsc" #-}
    Just AF_APPLETALK -> Just 5
{-# LINE 378 "Network/Socket/Types.hsc" #-}

{-# LINE 379 "Network/Socket/Types.hsc" #-}

{-# LINE 380 "Network/Socket/Types.hsc" #-}
    Just AF_ROUTE -> Just 16
{-# LINE 381 "Network/Socket/Types.hsc" #-}

{-# LINE 382 "Network/Socket/Types.hsc" #-}

{-# LINE 385 "Network/Socket/Types.hsc" #-}

{-# LINE 388 "Network/Socket/Types.hsc" #-}

{-# LINE 391 "Network/Socket/Types.hsc" #-}

{-# LINE 394 "Network/Socket/Types.hsc" #-}

{-# LINE 397 "Network/Socket/Types.hsc" #-}

{-# LINE 400 "Network/Socket/Types.hsc" #-}

{-# LINE 401 "Network/Socket/Types.hsc" #-}
    Just AF_X25 -> Just 9
{-# LINE 402 "Network/Socket/Types.hsc" #-}

{-# LINE 403 "Network/Socket/Types.hsc" #-}

{-# LINE 404 "Network/Socket/Types.hsc" #-}
    Just AF_AX25 -> Just 3
{-# LINE 405 "Network/Socket/Types.hsc" #-}

{-# LINE 406 "Network/Socket/Types.hsc" #-}

{-# LINE 409 "Network/Socket/Types.hsc" #-}

{-# LINE 412 "Network/Socket/Types.hsc" #-}

{-# LINE 413 "Network/Socket/Types.hsc" #-}
    Just AF_IPX -> Just 4
{-# LINE 414 "Network/Socket/Types.hsc" #-}

{-# LINE 415 "Network/Socket/Types.hsc" #-}

{-# LINE 418 "Network/Socket/Types.hsc" #-}

{-# LINE 421 "Network/Socket/Types.hsc" #-}

{-# LINE 424 "Network/Socket/Types.hsc" #-}

{-# LINE 427 "Network/Socket/Types.hsc" #-}

{-# LINE 430 "Network/Socket/Types.hsc" #-}

{-# LINE 433 "Network/Socket/Types.hsc" #-}

{-# LINE 436 "Network/Socket/Types.hsc" #-}

{-# LINE 439 "Network/Socket/Types.hsc" #-}

{-# LINE 442 "Network/Socket/Types.hsc" #-}

{-# LINE 445 "Network/Socket/Types.hsc" #-}

{-# LINE 448 "Network/Socket/Types.hsc" #-}

{-# LINE 451 "Network/Socket/Types.hsc" #-}

{-# LINE 452 "Network/Socket/Types.hsc" #-}
    Just AF_ISDN -> Just 34
{-# LINE 453 "Network/Socket/Types.hsc" #-}

{-# LINE 454 "Network/Socket/Types.hsc" #-}

{-# LINE 457 "Network/Socket/Types.hsc" #-}

{-# LINE 460 "Network/Socket/Types.hsc" #-}

{-# LINE 463 "Network/Socket/Types.hsc" #-}

{-# LINE 466 "Network/Socket/Types.hsc" #-}

{-# LINE 469 "Network/Socket/Types.hsc" #-}

{-# LINE 472 "Network/Socket/Types.hsc" #-}

{-# LINE 475 "Network/Socket/Types.hsc" #-}

{-# LINE 478 "Network/Socket/Types.hsc" #-}

{-# LINE 479 "Network/Socket/Types.hsc" #-}
    Just AF_NETROM -> Just 6
{-# LINE 480 "Network/Socket/Types.hsc" #-}

{-# LINE 481 "Network/Socket/Types.hsc" #-}

{-# LINE 482 "Network/Socket/Types.hsc" #-}
    Just AF_BRIDGE -> Just 7
{-# LINE 483 "Network/Socket/Types.hsc" #-}

{-# LINE 484 "Network/Socket/Types.hsc" #-}

{-# LINE 485 "Network/Socket/Types.hsc" #-}
    Just AF_ATMPVC -> Just 8
{-# LINE 486 "Network/Socket/Types.hsc" #-}

{-# LINE 487 "Network/Socket/Types.hsc" #-}

{-# LINE 488 "Network/Socket/Types.hsc" #-}
    Just AF_ROSE -> Just 11
{-# LINE 489 "Network/Socket/Types.hsc" #-}

{-# LINE 490 "Network/Socket/Types.hsc" #-}

{-# LINE 491 "Network/Socket/Types.hsc" #-}
    Just AF_NETBEUI -> Just 13
{-# LINE 492 "Network/Socket/Types.hsc" #-}

{-# LINE 493 "Network/Socket/Types.hsc" #-}

{-# LINE 494 "Network/Socket/Types.hsc" #-}
    Just AF_SECURITY -> Just 14
{-# LINE 495 "Network/Socket/Types.hsc" #-}

{-# LINE 496 "Network/Socket/Types.hsc" #-}

{-# LINE 497 "Network/Socket/Types.hsc" #-}
    Just AF_PACKET -> Just 17
{-# LINE 498 "Network/Socket/Types.hsc" #-}

{-# LINE 499 "Network/Socket/Types.hsc" #-}

{-# LINE 500 "Network/Socket/Types.hsc" #-}
    Just AF_ASH -> Just 18
{-# LINE 501 "Network/Socket/Types.hsc" #-}

{-# LINE 502 "Network/Socket/Types.hsc" #-}

{-# LINE 503 "Network/Socket/Types.hsc" #-}
    Just AF_ECONET -> Just 19
{-# LINE 504 "Network/Socket/Types.hsc" #-}

{-# LINE 505 "Network/Socket/Types.hsc" #-}

{-# LINE 506 "Network/Socket/Types.hsc" #-}
    Just AF_ATMSVC -> Just 20
{-# LINE 507 "Network/Socket/Types.hsc" #-}

{-# LINE 508 "Network/Socket/Types.hsc" #-}

{-# LINE 509 "Network/Socket/Types.hsc" #-}
    Just AF_IRDA -> Just 23
{-# LINE 510 "Network/Socket/Types.hsc" #-}

{-# LINE 511 "Network/Socket/Types.hsc" #-}

{-# LINE 512 "Network/Socket/Types.hsc" #-}
    Just AF_PPPOX -> Just 24
{-# LINE 513 "Network/Socket/Types.hsc" #-}

{-# LINE 514 "Network/Socket/Types.hsc" #-}

{-# LINE 515 "Network/Socket/Types.hsc" #-}
    Just AF_WANPIPE -> Just 25
{-# LINE 516 "Network/Socket/Types.hsc" #-}

{-# LINE 517 "Network/Socket/Types.hsc" #-}

{-# LINE 518 "Network/Socket/Types.hsc" #-}
    Just AF_BLUETOOTH -> Just 31
{-# LINE 519 "Network/Socket/Types.hsc" #-}

{-# LINE 520 "Network/Socket/Types.hsc" #-}

{-# LINE 521 "Network/Socket/Types.hsc" #-}
    Just AF_CAN -> Just 29
{-# LINE 522 "Network/Socket/Types.hsc" #-}

{-# LINE 523 "Network/Socket/Types.hsc" #-}
    _ -> Nothing

--------- ----------

unpackFamily :: CInt -> Family
unpackFamily f = case f of
        (0) -> AF_UNSPEC
{-# LINE 530 "Network/Socket/Types.hsc" #-}

{-# LINE 531 "Network/Socket/Types.hsc" #-}
        (1) -> AF_UNIX
{-# LINE 532 "Network/Socket/Types.hsc" #-}

{-# LINE 533 "Network/Socket/Types.hsc" #-}

{-# LINE 534 "Network/Socket/Types.hsc" #-}
        (2) -> AF_INET
{-# LINE 535 "Network/Socket/Types.hsc" #-}

{-# LINE 536 "Network/Socket/Types.hsc" #-}

{-# LINE 537 "Network/Socket/Types.hsc" #-}
        (10) -> AF_INET6
{-# LINE 538 "Network/Socket/Types.hsc" #-}

{-# LINE 539 "Network/Socket/Types.hsc" #-}

{-# LINE 542 "Network/Socket/Types.hsc" #-}

{-# LINE 545 "Network/Socket/Types.hsc" #-}

{-# LINE 548 "Network/Socket/Types.hsc" #-}

{-# LINE 551 "Network/Socket/Types.hsc" #-}

{-# LINE 554 "Network/Socket/Types.hsc" #-}

{-# LINE 557 "Network/Socket/Types.hsc" #-}

{-# LINE 560 "Network/Socket/Types.hsc" #-}

{-# LINE 563 "Network/Socket/Types.hsc" #-}

{-# LINE 564 "Network/Socket/Types.hsc" #-}
        (22) -> AF_SNA
{-# LINE 565 "Network/Socket/Types.hsc" #-}

{-# LINE 566 "Network/Socket/Types.hsc" #-}

{-# LINE 567 "Network/Socket/Types.hsc" #-}
        (12) -> AF_DECnet
{-# LINE 568 "Network/Socket/Types.hsc" #-}

{-# LINE 569 "Network/Socket/Types.hsc" #-}

{-# LINE 572 "Network/Socket/Types.hsc" #-}

{-# LINE 575 "Network/Socket/Types.hsc" #-}

{-# LINE 578 "Network/Socket/Types.hsc" #-}

{-# LINE 579 "Network/Socket/Types.hsc" #-}
        (5) -> AF_APPLETALK
{-# LINE 580 "Network/Socket/Types.hsc" #-}

{-# LINE 581 "Network/Socket/Types.hsc" #-}

{-# LINE 582 "Network/Socket/Types.hsc" #-}
        (16) -> AF_ROUTE
{-# LINE 583 "Network/Socket/Types.hsc" #-}

{-# LINE 584 "Network/Socket/Types.hsc" #-}

{-# LINE 587 "Network/Socket/Types.hsc" #-}

{-# LINE 590 "Network/Socket/Types.hsc" #-}

{-# LINE 593 "Network/Socket/Types.hsc" #-}

{-# LINE 596 "Network/Socket/Types.hsc" #-}

{-# LINE 601 "Network/Socket/Types.hsc" #-}

{-# LINE 604 "Network/Socket/Types.hsc" #-}

{-# LINE 605 "Network/Socket/Types.hsc" #-}
        (9) -> AF_X25
{-# LINE 606 "Network/Socket/Types.hsc" #-}

{-# LINE 607 "Network/Socket/Types.hsc" #-}

{-# LINE 608 "Network/Socket/Types.hsc" #-}
        (3) -> AF_AX25
{-# LINE 609 "Network/Socket/Types.hsc" #-}

{-# LINE 610 "Network/Socket/Types.hsc" #-}

{-# LINE 613 "Network/Socket/Types.hsc" #-}

{-# LINE 616 "Network/Socket/Types.hsc" #-}

{-# LINE 617 "Network/Socket/Types.hsc" #-}
        (4) -> AF_IPX
{-# LINE 618 "Network/Socket/Types.hsc" #-}

{-# LINE 619 "Network/Socket/Types.hsc" #-}

{-# LINE 622 "Network/Socket/Types.hsc" #-}

{-# LINE 625 "Network/Socket/Types.hsc" #-}

{-# LINE 628 "Network/Socket/Types.hsc" #-}

{-# LINE 631 "Network/Socket/Types.hsc" #-}

{-# LINE 634 "Network/Socket/Types.hsc" #-}

{-# LINE 637 "Network/Socket/Types.hsc" #-}

{-# LINE 640 "Network/Socket/Types.hsc" #-}

{-# LINE 643 "Network/Socket/Types.hsc" #-}

{-# LINE 646 "Network/Socket/Types.hsc" #-}

{-# LINE 649 "Network/Socket/Types.hsc" #-}

{-# LINE 652 "Network/Socket/Types.hsc" #-}

{-# LINE 655 "Network/Socket/Types.hsc" #-}

{-# LINE 656 "Network/Socket/Types.hsc" #-}
        (34) -> AF_ISDN
{-# LINE 657 "Network/Socket/Types.hsc" #-}

{-# LINE 658 "Network/Socket/Types.hsc" #-}

{-# LINE 661 "Network/Socket/Types.hsc" #-}

{-# LINE 664 "Network/Socket/Types.hsc" #-}

{-# LINE 667 "Network/Socket/Types.hsc" #-}

{-# LINE 670 "Network/Socket/Types.hsc" #-}

{-# LINE 673 "Network/Socket/Types.hsc" #-}

{-# LINE 676 "Network/Socket/Types.hsc" #-}

{-# LINE 679 "Network/Socket/Types.hsc" #-}

{-# LINE 682 "Network/Socket/Types.hsc" #-}

{-# LINE 683 "Network/Socket/Types.hsc" #-}
        (6) -> AF_NETROM
{-# LINE 684 "Network/Socket/Types.hsc" #-}

{-# LINE 685 "Network/Socket/Types.hsc" #-}

{-# LINE 686 "Network/Socket/Types.hsc" #-}
        (7) -> AF_BRIDGE
{-# LINE 687 "Network/Socket/Types.hsc" #-}

{-# LINE 688 "Network/Socket/Types.hsc" #-}

{-# LINE 689 "Network/Socket/Types.hsc" #-}
        (8) -> AF_ATMPVC
{-# LINE 690 "Network/Socket/Types.hsc" #-}

{-# LINE 691 "Network/Socket/Types.hsc" #-}

{-# LINE 692 "Network/Socket/Types.hsc" #-}
        (11) -> AF_ROSE
{-# LINE 693 "Network/Socket/Types.hsc" #-}

{-# LINE 694 "Network/Socket/Types.hsc" #-}

{-# LINE 695 "Network/Socket/Types.hsc" #-}
        (13) -> AF_NETBEUI
{-# LINE 696 "Network/Socket/Types.hsc" #-}

{-# LINE 697 "Network/Socket/Types.hsc" #-}

{-# LINE 698 "Network/Socket/Types.hsc" #-}
        (14) -> AF_SECURITY
{-# LINE 699 "Network/Socket/Types.hsc" #-}

{-# LINE 700 "Network/Socket/Types.hsc" #-}

{-# LINE 701 "Network/Socket/Types.hsc" #-}
        (17) -> AF_PACKET
{-# LINE 702 "Network/Socket/Types.hsc" #-}

{-# LINE 703 "Network/Socket/Types.hsc" #-}

{-# LINE 704 "Network/Socket/Types.hsc" #-}
        (18) -> AF_ASH
{-# LINE 705 "Network/Socket/Types.hsc" #-}

{-# LINE 706 "Network/Socket/Types.hsc" #-}

{-# LINE 707 "Network/Socket/Types.hsc" #-}
        (19) -> AF_ECONET
{-# LINE 708 "Network/Socket/Types.hsc" #-}

{-# LINE 709 "Network/Socket/Types.hsc" #-}

{-# LINE 710 "Network/Socket/Types.hsc" #-}
        (20) -> AF_ATMSVC
{-# LINE 711 "Network/Socket/Types.hsc" #-}

{-# LINE 712 "Network/Socket/Types.hsc" #-}

{-# LINE 713 "Network/Socket/Types.hsc" #-}
        (23) -> AF_IRDA
{-# LINE 714 "Network/Socket/Types.hsc" #-}

{-# LINE 715 "Network/Socket/Types.hsc" #-}

{-# LINE 716 "Network/Socket/Types.hsc" #-}
        (24) -> AF_PPPOX
{-# LINE 717 "Network/Socket/Types.hsc" #-}

{-# LINE 718 "Network/Socket/Types.hsc" #-}

{-# LINE 719 "Network/Socket/Types.hsc" #-}
        (25) -> AF_WANPIPE
{-# LINE 720 "Network/Socket/Types.hsc" #-}

{-# LINE 721 "Network/Socket/Types.hsc" #-}

{-# LINE 722 "Network/Socket/Types.hsc" #-}
        (31) -> AF_BLUETOOTH
{-# LINE 723 "Network/Socket/Types.hsc" #-}

{-# LINE 724 "Network/Socket/Types.hsc" #-}

{-# LINE 725 "Network/Socket/Types.hsc" #-}
        (29) -> AF_CAN
{-# LINE 726 "Network/Socket/Types.hsc" #-}

{-# LINE 727 "Network/Socket/Types.hsc" #-}
        unknown -> error $
          "Network.Socket.Types.unpackFamily: unknown address family: " ++
          show unknown

------------------------------------------------------------------------
-- Port Numbers

-- | Use the @Num@ instance (i.e. use a literal) to create a
-- @PortNumber@ value with the correct network-byte-ordering. You
-- should not use the PortNum constructor. It will be removed in the
-- next release.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable)
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.

{-# DEPRECATED PortNum "Do not use the PortNum constructor. Use the Num instance. PortNum will be removed in the next release." #-}

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (intToPortNumber x, y)) . readsPrec n

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import ccall unsafe "htons" htons :: Word16 -> Word16
foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall unsafe "htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
                  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `liftM` peek (castPtr p)

------------------------------------------------------------------------
-- Socket addresses

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for Unix domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only Unix domain sockets and the Internet
-- families are supported.


{-# LINE 816 "Network/Socket/Types.hsc" #-}
type FlowInfo = Word32
type ScopeID = Word32

{-# LINE 819 "Network/Socket/Types.hsc" #-}

-- | The existence of a constructor does not necessarily imply that
-- that socket address type is supported on your system: see
-- 'isSupportedSockAddr'.
data SockAddr       -- C Names
  = SockAddrInet
    PortNumber  -- sin_port  (network byte order)
    HostAddress -- sin_addr  (ditto)
  | SockAddrInet6
        PortNumber      -- sin6_port (network byte order)
        FlowInfo        -- sin6_flowinfo (ditto)
        HostAddress6    -- sin6_addr (ditto)
        ScopeID         -- sin6_scope_id (ditto)
  | SockAddrUnix
        String          -- sun_path
  | SockAddrCan
        Int32           -- can_ifindex (can be get by Network.BSD.ifNameToIndex "can0")
        -- TODO: Extend this to include transport protocol information
  deriving (Eq, Ord, Typeable)

-- | Is the socket address type supported on this system?
isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr addr = case addr of
  SockAddrInet {} -> True

{-# LINE 844 "Network/Socket/Types.hsc" #-}
  SockAddrInet6 {} -> True

{-# LINE 846 "Network/Socket/Types.hsc" #-}

{-# LINE 847 "Network/Socket/Types.hsc" #-}
  SockAddrUnix{} -> True

{-# LINE 849 "Network/Socket/Types.hsc" #-}

{-# LINE 850 "Network/Socket/Types.hsc" #-}
  SockAddrCan{} -> True

{-# LINE 852 "Network/Socket/Types.hsc" #-}

{-# LINE 856 "Network/Socket/Types.hsc" #-}


{-# LINE 862 "Network/Socket/Types.hsc" #-}
type CSaFamily = (Word16)
{-# LINE 863 "Network/Socket/Types.hsc" #-}

{-# LINE 864 "Network/Socket/Types.hsc" #-}

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> Int

{-# LINE 870 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr (SockAddrUnix path) =
    case path of
        '\0':_ -> (2) + length path
{-# LINE 873 "Network/Socket/Types.hsc" #-}
        _      -> 110
{-# LINE 874 "Network/Socket/Types.hsc" #-}

{-# LINE 875 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr (SockAddrInet _ _) = 16
{-# LINE 876 "Network/Socket/Types.hsc" #-}

{-# LINE 877 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr (SockAddrInet6 _ _ _ _) = 28
{-# LINE 878 "Network/Socket/Types.hsc" #-}

{-# LINE 879 "Network/Socket/Types.hsc" #-}

{-# LINE 880 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr (SockAddrCan _) = 16
{-# LINE 881 "Network/Socket/Types.hsc" #-}

{-# LINE 882 "Network/Socket/Types.hsc" #-}

-- | Computes the storage requirements (in bytes) required for a
-- 'SockAddr' with the given 'Family'.
sizeOfSockAddrByFamily :: Family -> Int

{-# LINE 887 "Network/Socket/Types.hsc" #-}
sizeOfSockAddrByFamily AF_UNIX  = 110
{-# LINE 888 "Network/Socket/Types.hsc" #-}

{-# LINE 889 "Network/Socket/Types.hsc" #-}

{-# LINE 890 "Network/Socket/Types.hsc" #-}
sizeOfSockAddrByFamily AF_INET6 = 28
{-# LINE 891 "Network/Socket/Types.hsc" #-}

{-# LINE 892 "Network/Socket/Types.hsc" #-}
sizeOfSockAddrByFamily AF_INET  = 16
{-# LINE 893 "Network/Socket/Types.hsc" #-}

{-# LINE 894 "Network/Socket/Types.hsc" #-}
sizeOfSockAddrByFamily AF_CAN   = 16
{-# LINE 895 "Network/Socket/Types.hsc" #-}

{-# LINE 896 "Network/Socket/Types.hsc" #-}
sizeOfSockAddrByFamily family = error $
    "Network.Socket.Types.sizeOfSockAddrByFamily: address family '" ++
    show family ++ "' not supported."

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- 'SockAddr' and the length of that 'SockAddr'.
withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- | Create a new 'SockAddr' for use with a function requiring a
-- pointer to a 'SockAddr' and the length of that 'SockAddr'.
withNewSockAddr :: Family -> (Ptr SockAddr -> Int -> IO a) -> IO a
withNewSockAddr family f = do
    let sz = sizeOfSockAddrByFamily family
    allocaBytes sz $ \ptr -> f ptr sz

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.

-- | Write the given 'SockAddr' to the given memory location.
pokeSockAddr :: Ptr a -> SockAddr -> IO ()

{-# LINE 924 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrUnix path) = do

{-# LINE 928 "Network/Socket/Types.hsc" #-}
    case path of
      ('\0':_) -> zeroMemory p (110)
{-# LINE 930 "Network/Socket/Types.hsc" #-}
      _        -> return ()

{-# LINE 932 "Network/Socket/Types.hsc" #-}

{-# LINE 935 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((1) :: CSaFamily)
{-# LINE 936 "Network/Socket/Types.hsc" #-}
    let pathC = map castCharToCChar path
        poker = case path of ('\0':_) -> pokeArray; _ -> pokeArray0 0
    poker (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p) pathC
{-# LINE 939 "Network/Socket/Types.hsc" #-}

{-# LINE 940 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet (PortNum port) addr) = do

{-# LINE 944 "Network/Socket/Types.hsc" #-}

{-# LINE 947 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((2) :: CSaFamily)
{-# LINE 948 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 949 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p addr
{-# LINE 950 "Network/Socket/Types.hsc" #-}

{-# LINE 951 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet6 (PortNum port) flow addr scope) = do

{-# LINE 955 "Network/Socket/Types.hsc" #-}

{-# LINE 958 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((10) :: CSaFamily)
{-# LINE 959 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 960 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p flow
{-# LINE 961 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (In6Addr addr)
{-# LINE 962 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p scope
{-# LINE 963 "Network/Socket/Types.hsc" #-}

{-# LINE 964 "Network/Socket/Types.hsc" #-}

{-# LINE 965 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrCan ifIndex) = do

{-# LINE 969 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p ifIndex
{-# LINE 970 "Network/Socket/Types.hsc" #-}

{-# LINE 971 "Network/Socket/Types.hsc" #-}

-- | Read a 'SockAddr' from the given memory location.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
  family <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 976 "Network/Socket/Types.hsc" #-}
  case family :: CSaFamily of

{-# LINE 978 "Network/Socket/Types.hsc" #-}
    (1) -> do
{-# LINE 979 "Network/Socket/Types.hsc" #-}
        str <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p)
{-# LINE 980 "Network/Socket/Types.hsc" #-}
        return (SockAddrUnix str)

{-# LINE 982 "Network/Socket/Types.hsc" #-}
    (2) -> do
{-# LINE 983 "Network/Socket/Types.hsc" #-}
        addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 984 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 985 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet (PortNum port) addr)

{-# LINE 987 "Network/Socket/Types.hsc" #-}
    (10) -> do
{-# LINE 988 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 989 "Network/Socket/Types.hsc" #-}
        flow <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 990 "Network/Socket/Types.hsc" #-}
        In6Addr addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 991 "Network/Socket/Types.hsc" #-}
        scope <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 992 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet6 (PortNum port) flow addr scope)

{-# LINE 994 "Network/Socket/Types.hsc" #-}

{-# LINE 995 "Network/Socket/Types.hsc" #-}
    (29) -> do
{-# LINE 996 "Network/Socket/Types.hsc" #-}
        ifidx <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 997 "Network/Socket/Types.hsc" #-}
        return (SockAddrCan ifidx)

{-# LINE 999 "Network/Socket/Types.hsc" #-}
    _ -> ioError $ userError $
      "Network.Socket.Types.peekSockAddr: address family '" ++
      show family ++ "' not supported."

------------------------------------------------------------------------

-- | The raw network byte order number is read using host byte order.
-- Therefore on little-endian architectures the byte order is swapped. For
-- example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
-- and as @0x7f000001@ on big-endian hosts.
--
-- For direct manipulation prefer 'hostAddressToTuple' and
-- 'tupleToHostAddress'.
type HostAddress = Word32

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)


{-# LINE 1030 "Network/Socket/Types.hsc" #-}
-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'hostAddress6ToTuple' and
-- 'tupleToHostAddress6'.
type HostAddress6 = (Word32, Word32, Word32, Word32)

hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (w3, w2, w1, w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = ((0))
{-# LINE 1057 "Network/Socket/Types.hsc" #-}

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

-- | Private newtype proxy for the Storable instance. To avoid orphan instances.
newtype In6Addr = In6Addr HostAddress6


{-# LINE 1085 "Network/Socket/Types.hsc" #-}

instance Storable In6Addr where
    sizeOf _    = 16
{-# LINE 1088 "Network/Socket/Types.hsc" #-}
    alignment _ = (4)
{-# LINE 1089 "Network/Socket/Types.hsc" #-}

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ In6Addr (a, b, c, d)

    poke p (In6Addr (a, b, c, d)) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

{-# LINE 1103 "Network/Socket/Types.hsc" #-}

------------------------------------------------------------------------
-- Helper functions

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
