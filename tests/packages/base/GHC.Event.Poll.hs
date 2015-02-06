{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/GHC/Event/Poll.hs" #-}
{-# LINE 1 "GHC/Event/Poll.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "GHC/Event/Poll.hsc" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , BangPatterns
  #-}

module GHC.Event.Poll
    (
      new
    , available
    ) where


{-# LINE 14 "GHC/Event/Poll.hsc" #-}


{-# LINE 25 "GHC/Event/Poll.hsc" #-}

{-# LINE 26 "GHC/Event/Poll.hsc" #-}

import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad ((=<<), liftM, liftM2, unless)
import Data.Bits (Bits, FiniteBits, (.|.), (.&.))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Word
import Foreign.C.Types (CInt(..), CShort(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Base
import GHC.Conc.Sync (withMVar)
import GHC.Enum (maxBound)
import GHC.Num (Num(..))
import GHC.Real (ceiling, fromIntegral)
import GHC.Show (Show)
import System.Posix.Types (Fd(..))

import qualified GHC.Event.Array as A
import qualified GHC.Event.Internal as E

available :: Bool
available = True
{-# INLINE available #-}

data Poll = Poll {
      pollChanges :: {-# UNPACK #-} !(MVar (A.Array PollFd))
    , pollFd      :: {-# UNPACK #-} !(A.Array PollFd)
    }

new :: IO E.Backend
new = E.backend poll modifyFd modifyFdOnce (\_ -> return ()) `liftM`
      liftM2 Poll (newMVar =<< A.empty) A.empty

modifyFd :: Poll -> Fd -> E.Event -> E.Event -> IO Bool
modifyFd p fd oevt nevt =
  withMVar (pollChanges p) $ \ary -> do
    A.snoc ary $ PollFd fd (fromEvent nevt) (fromEvent oevt)
    return True

modifyFdOnce :: Poll -> Fd -> E.Event -> IO Bool
modifyFdOnce = error "modifyFdOnce not supported in Poll backend"

reworkFd :: Poll -> PollFd -> IO ()
reworkFd p (PollFd fd npevt opevt) = do
  let ary = pollFd p
  if opevt == 0
    then A.snoc ary $ PollFd fd npevt 0
    else do
      found <- A.findIndex ((== fd) . pfdFd) ary
      case found of
        Nothing        -> error "reworkFd: event not found"
        Just (i,_)
          | npevt /= 0 -> A.unsafeWrite ary i $ PollFd fd npevt 0
          | otherwise  -> A.removeAt ary i

poll :: Poll
     -> Maybe E.Timeout
     -> (Fd -> E.Event -> IO ())
     -> IO Int
poll p mtout f = do
  let a = pollFd p
  mods <- swapMVar (pollChanges p) =<< A.empty
  A.forM_ mods (reworkFd p)
  n <- A.useAsPtr a $ \ptr len ->
    E.throwErrnoIfMinus1NoRetry "c_poll" $
    case mtout of
      Just tout ->
        c_pollLoop ptr (fromIntegral len) (fromTimeout tout)
      Nothing   ->
        c_poll_unsafe ptr (fromIntegral len) 0
  unless (n == 0) $ do
    A.loop a 0 $ \i e -> do
      let r = pfdRevents e
      if r /= 0
        then do f (pfdFd e) (toEvent r)
                let i' = i + 1
                return (i', i' == n)
        else return (i, True)
  return (fromIntegral n)
  where
    -- The poll timeout is specified as an Int, but c_poll takes a CInt. These
    -- can't be safely coerced as on many systems (e.g. x86_64) CInt has a
    -- maxBound of (2^32 - 1), even though Int may have a significantly higher
    -- bound.
    --
    -- This function deals with timeouts greater than maxBound :: CInt, by
    -- looping until c_poll returns a non-zero value (0 indicates timeout
    -- expired) OR the full timeout has passed.
    c_pollLoop :: Ptr PollFd -> (Word64) -> Int -> IO CInt
{-# LINE 116 "GHC/Event/Poll.hsc" #-}
    c_pollLoop ptr len tout
        | tout <= maxPollTimeout = c_poll ptr len (fromIntegral tout)
        | otherwise = do
            result <- c_poll ptr len (fromIntegral maxPollTimeout)
            if result == 0
               then c_pollLoop ptr len (fromIntegral (tout - maxPollTimeout))
               else return result

    -- We need to account for 3 cases:
    --     1. Int and CInt are of equal size.
    --     2. Int is larger than CInt
    --     3. Int is smaller than CInt
    --
    -- In case 1, the value of maxPollTimeout will be the maxBound of Int.
    --
    -- In case 2, the value of maxPollTimeout will be the maxBound of CInt,
    -- which is the largest value accepted by c_poll. This will result in
    -- c_pollLoop recursing if the provided timeout is larger.
    --
    -- In case 3, "fromIntegral (maxBound :: CInt) :: Int" will result in a
    -- negative Int, max will thus return maxBound :: Int. Since poll doesn't
    -- accept values bigger than maxBound :: Int and CInt is larger than Int,
    -- there is no problem converting Int to CInt for the c_poll call.
    maxPollTimeout :: Int
    maxPollTimeout = max maxBound (fromIntegral (maxBound :: CInt))

fromTimeout :: E.Timeout -> Int
fromTimeout E.Forever     = -1
fromTimeout (E.Timeout s) = ceiling $ 1000 * s

data PollFd = PollFd {
      pfdFd      :: {-# UNPACK #-} !Fd
    , pfdEvents  :: {-# UNPACK #-} !Event
    , pfdRevents :: {-# UNPACK #-} !Event
    } deriving (Show)

newtype Event = Event CShort
    deriving (Eq, Show, Num, Storable, Bits, FiniteBits)

-- We have to duplicate the whole enum like this in order for the
-- hsc2hs cross-compilation mode to work

{-# LINE 166 "GHC/Event/Poll.hsc" #-}
pollIn     :: Event
pollIn     = Event 1
pollOut    :: Event
pollOut    = Event 4
pollErr    :: Event
pollErr    = Event 8
pollHup    :: Event
pollHup    = Event 16

{-# LINE 172 "GHC/Event/Poll.hsc" #-}

{-# LINE 173 "GHC/Event/Poll.hsc" #-}

fromEvent :: E.Event -> Event
fromEvent e = remap E.evtRead  pollIn .|.
              remap E.evtWrite pollOut
  where remap evt to
            | e `E.eventIs` evt = to
            | otherwise         = 0

toEvent :: Event -> E.Event
toEvent e = remap (pollIn .|. pollErr .|. pollHup)  E.evtRead `mappend`
            remap (pollOut .|. pollErr .|. pollHup) E.evtWrite
  where remap evt to
            | e .&. evt /= 0 = to
            | otherwise      = mempty

instance Storable PollFd where
    sizeOf _    = (8)
{-# LINE 190 "GHC/Event/Poll.hsc" #-}
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
      fd <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 194 "GHC/Event/Poll.hsc" #-}
      events <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 195 "GHC/Event/Poll.hsc" #-}
      revents <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 196 "GHC/Event/Poll.hsc" #-}
      let !pollFd' = PollFd fd events revents
      return pollFd'

    poke ptr p = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (pfdFd p)
{-# LINE 201 "GHC/Event/Poll.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (pfdEvents p)
{-# LINE 202 "GHC/Event/Poll.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr (pfdRevents p)
{-# LINE 203 "GHC/Event/Poll.hsc" #-}

foreign import ccall safe "poll.h poll"
    c_poll :: Ptr PollFd -> (Word64) -> CInt -> IO CInt
{-# LINE 206 "GHC/Event/Poll.hsc" #-}

foreign import ccall unsafe "poll.h poll"
    c_poll_unsafe :: Ptr PollFd -> (Word64) -> CInt -> IO CInt
{-# LINE 209 "GHC/Event/Poll.hsc" #-}

{-# LINE 210 "GHC/Event/Poll.hsc" #-}
