{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/Binary/Get/Internal.hs" #-}















































{-# LANGUAGE CPP, RankNTypes, MagicHash, BangPatterns, TypeFamilies #-}

-- CPP C style pre-precessing, the #if defined lines
-- RankNTypes forall r. statement
-- MagicHash the (# unboxing #), also needs GHC.primitives

module Data.Binary.Get.Internal (

    -- * The Get type
      Get
    , runCont
    , Decoder(..)
    , runGetIncremental

    , readN
    , readNWith

    -- * Parsing
    , skip
    , bytesRead
    , isolate
    
    , get
    , put
    , demandInput
    , ensureN

    -- * Utility
    , remaining
    , getBytes
    , isEmpty
    , lookAhead
    , lookAheadM
    , lookAheadE
    , label

    -- ** ByteStrings
    , getByteString

    ) where

import Foreign
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import Control.Applicative
import Control.Monad


-- Kolmodin 20100427: at zurihac we discussed of having partial take a
-- "Maybe ByteString" and implemented it in this way.
-- The reasoning was that you could accidently provide an empty bytestring,
-- and it should not terminate the decoding (empty would mean eof).
-- However, I'd say that it's also a risk that you get stuck in a loop,
-- where you keep providing an empty string. Anyway, no new input should be
-- rare, as the RTS should only wake you up if you actually have some data
-- to read from your fd.

-- | A decoder produced by running a 'Get' monad.
data Decoder a = Fail !B.ByteString String
              -- ^ The decoder ran into an error. The decoder either used
              -- 'fail' or was not provided enough input.
              | Partial (Maybe B.ByteString -> Decoder a)
              -- ^ The decoder has consumed the available input and needs
              -- more to continue. Provide 'Just' if more input is available
              -- and 'Nothing' otherwise, and you will get a new 'Decoder'.
              | Done !B.ByteString a
              -- ^ The decoder has successfully finished. Except for the
              -- output value you also get the unused input.
              | BytesRead {-# UNPACK #-} !Int64 (Int64 -> Decoder a)
              -- ^ The decoder needs to know the current position in the input.
              -- Given the number of bytes remaning in the decoder, the outer
              -- decoder runner needs to calculate the position and
              -- resume the decoding.

-- unrolled codensity/state monad
newtype Get a = C { runCont :: forall r.
                               B.ByteString ->
                               Success a r ->
                               Decoder   r }

type Success a r = B.ByteString -> a -> Decoder r

instance Monad Get where
  return = returnG
  (>>=) = bindG
  fail = failG

returnG :: a -> Get a
returnG a = C $ \s ks -> ks s a
{-# INLINE [0] returnG #-}

bindG :: Get a -> (a -> Get b) -> Get b
bindG (C c) f = C $ \i ks -> c i (\i' a -> (runCont (f a)) i' ks)
{-# INLINE bindG #-}

failG :: String -> Get a
failG str = C $ \i _ks -> Fail i str

apG :: Get (a -> b) -> Get a -> Get b
apG d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE [0] apG #-}

fmapG :: (a -> b) -> Get a -> Get b
fmapG f m = C $ \i ks -> runCont m i (\i' a -> ks i' (f a))
{-# INLINE fmapG #-}

instance Applicative Get where
  pure = returnG
  {-# INLINE pure #-}
  (<*>) = apG
  {-# INLINE (<*>) #-}

instance MonadPlus Get where
  mzero = empty
  mplus = (<|>)

instance Functor Get where
  fmap = fmapG

instance Functor Decoder where
  fmap f (Done s a) = Done s (f a)
  fmap f (Partial k) = Partial (fmap f . k)
  fmap _ (Fail s msg) = Fail s msg
  fmap f (BytesRead b k) = BytesRead b (fmap f . k)

instance (Show a) => Show (Decoder a) where
  show (Fail _ msg) = "Fail: " ++ msg
  show (Partial _) = "Partial _"
  show (Done _ a) = "Done: " ++ show a
  show (BytesRead _ _) = "BytesRead"

-- | Run a 'Get' monad. See 'Decoder' for what to do next, like providing
-- input, handling decoding errors and to get the output value.
runGetIncremental :: Get a -> Decoder a
runGetIncremental g = noMeansNo $
  runCont g B.empty (\i a -> Done i a)

-- | Make sure we don't have to pass Nothing to a Partial twice.
-- This way we don't need to pass around an EOF value in the Get monad, it
-- can safely ask several times if it needs to.
noMeansNo :: Decoder a -> Decoder a
noMeansNo r0 = go r0
  where
  go r =
    case r of
      Partial k -> Partial $ \ms ->
                    case ms of
                      Just _ -> go (k ms)
                      Nothing -> neverAgain (k ms)
      BytesRead n k -> BytesRead n (go . k)
      Done _ _ -> r
      Fail _ _ -> r
  neverAgain r =
    case r of
      Partial k -> neverAgain (k Nothing)
      BytesRead n k -> BytesRead n (neverAgain . k)
      Fail _ _ -> r
      Done _ _ -> r

prompt :: B.ByteString -> Decoder a -> (B.ByteString -> Decoder a) -> Decoder a
prompt inp kf ks =
    let loop =
         Partial $ \sm ->
           case sm of
             Just s | B.null s -> loop
                    | otherwise -> ks (inp `B.append` s)
             Nothing -> kf
    in loop

-- | Get the total number of bytes read to this point.
bytesRead :: Get Int64
bytesRead = C $ \inp k -> BytesRead (fromIntegral $ B.length inp) (k inp)

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or more bytes were attempted to be consumed.
-- If the given decoder fails, 'isolate' will also fail.
-- Offset from 'bytesRead' will be relative to the start of 'isolate', not the
-- absolute of the input.
isolate :: Int   -- ^ The number of bytes that must be consumed
        -> Get a -- ^ The decoder to isolate
        -> Get a
isolate n0 act
  | n0 < 0 = fail "isolate: negative size"
  | otherwise = go n0 (runCont act B.empty Done)
  where
  go !n (Done left x)
    | n == 0 && B.null left = return x
    | otherwise = do
        pushFront left
        let consumed = n0 - n - B.length left
        fail $ "isolate: the decoder consumed " ++ show consumed ++ " bytes" ++
                 " which is less than the expected " ++ show n0 ++ " bytes"
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- C $ \inp k -> do
      let takeLimited str =
            let (inp', out) = B.splitAt n str
            in k out (Just inp')
      case not (B.null inp) of
        True -> takeLimited inp
        False -> prompt inp (k B.empty Nothing) takeLimited
    case inp of
      Nothing -> go n (resume Nothing)
      Just str -> go (n - B.length str) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! fromIntegral n0 - fromIntegral n - r)

-- | Demand more input. If none available, fail.
demandInput :: Get ()
demandInput = C $ \inp ks ->
  prompt inp (Fail inp "demandInput: not enough bytes") (\inp' -> ks inp' ())

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = readN n (const ())
{-# INLINE skip #-}

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: Get Bool
isEmpty = C $ \inp ks ->
    if B.null inp
      then prompt inp (ks inp True) (\inp' -> ks inp' False)
      else ks inp False

-- | DEPRECATED. Same as 'getByteString'.
{-# DEPRECATED getBytes "Use 'getByteString' instead of 'getBytes'." #-}
getBytes :: Int -> Get B.ByteString
getBytes = getByteString
{-# INLINE getBytes #-}

instance Alternative Get where
  empty = C $ \inp _ks -> Fail inp "Data.Binary.Get(Alternative).empty"
  (<|>) f g = do
    (decoder, bs) <- runAndKeepTrack f
    case decoder of
      Done inp x -> C $ \_ ks -> ks inp x
      Fail _ _ -> pushBack bs >> g
      _ -> error "Binary: impossible"
  some p = (:) <$> p <*> many p
  many p = do
    v <- (Just <$> p) <|> pure Nothing
    case v of
      Nothing -> pure []
      Just x -> (:) x <$> many p

-- | Run a decoder and keep track of all the input it consumes.
-- Once it's finished, return the final decoder (always 'Done' or 'Fail'), 
-- and unconsume all the the input the decoder required to run.
-- Any additional chunks which was required to run the decoder
-- will also be returned.
runAndKeepTrack :: Get a -> Get (Decoder a, [B.ByteString])
runAndKeepTrack g = C $ \inp ks ->
  let r0 = runCont g inp (\inp' a -> Done inp' a)
      go !acc r = case r of
                    Done inp' a -> ks inp (Done inp' a, reverse acc)
                    Partial k -> Partial $ \minp -> go (maybe acc (:acc) minp) (k minp)
                    Fail inp' s -> ks inp (Fail inp' s, reverse acc)
                    BytesRead unused k -> BytesRead unused (go acc . k)
  in go [] r0
{-# INLINE runAndKeepTrack #-}

pushBack :: [B.ByteString] -> Get ()
pushBack [] = C $ \ inp ks -> ks inp ()
pushBack bs = C $ \ inp ks -> ks (B.concat (inp : bs)) ()
{-# INLINE pushBack #-}

pushFront :: B.ByteString -> Get ()
pushFront bs = C $ \ inp ks -> ks (B.append bs inp) ()
{-# INLINE pushFront #-}

-- | Run the given decoder, but without consuming its input. If the given
-- decoder fails, then so will this function.
lookAhead :: Get a -> Get a
lookAhead g = do
  (decoder, bs) <- runAndKeepTrack g
  case decoder of
    Done _ a -> pushBack bs >> return a
    Fail inp s -> C $ \_ _ -> Fail inp s
    _ -> error "Binary: impossible"

-- | Run the given decoder, and only consume its input if it returns 'Just'.
-- If 'Nothing' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM g = do
  let g' = maybe (Left ()) Right <$> g
  either (const Nothing) Just <$> lookAheadE g'

-- | Run the given decoder, and only consume its input if it returns 'Right'.
-- If 'Left' is returned, the input will be unconsumed.
-- If the given decoder fails, then so will this function.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE g = do
  (decoder, bs) <- runAndKeepTrack g
  case decoder of
    Done _ (Left x) -> pushBack bs >> return (Left x)
    Done inp (Right x) -> C $ \_ ks -> ks inp (Right x)
    Fail inp s -> C $ \_ _ -> Fail inp s
    _ -> error "Binary: impossible"

-- Label a decoder. If the decoder fails, the label will be appended on
-- a new line to the error message string.
label :: String -> Get a -> Get a
label msg decoder = C $ \inp ks ->
  let r0 = runCont decoder inp (\inp' a -> Done inp' a)
      go r = case r of
                 Done inp' a -> ks inp' a
                 Partial k -> Partial (go . k)
                 Fail inp' s -> Fail inp' (s ++ "\n" ++ msg)
                 BytesRead u k -> BytesRead u (go . k)
  in go r0

-- | DEPRECATED. Get the number of bytes of remaining input.
-- Note that this is an expensive function to use as in order to calculate how
-- much input remains, all input has to be read and kept in-memory.
-- The decoder keeps the input as a strict bytestring, so you are likely better
-- off by calculating the remaining input in another way.
{-# DEPRECATED remaining "This will force all remaining input, don't use it." #-}
remaining :: Get Int64
remaining = C $ \ inp ks ->
  let loop acc = Partial $ \ minp ->
                  case minp of
                    Nothing -> let all_inp = B.concat (inp : (reverse acc))
                               in ks all_inp (fromIntegral $ B.length all_inp)
                    Just inp' -> loop (inp':acc)
  in loop []

------------------------------------------------------------------------
-- ByteStrings
--

-- | An efficient get method for strict ByteStrings. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Int -> Get B.ByteString
getByteString n | n > 0 = readN n (B.unsafeTake n)
                | otherwise = return B.empty
{-# INLINE getByteString #-}

-- | Get the current chunk.
get :: Get B.ByteString
get = C $ \inp ks -> ks inp inp

-- | Replace the current chunk.
put :: B.ByteString -> Get ()
put s = C $ \_inp ks -> ks s ()

-- | Return at least @n@ bytes, maybe more. If not enough data is available
-- the computation will escape with 'Partial'.
readN :: Int -> (B.ByteString -> a) -> Get a
readN !n f = ensureN n >> unsafeReadN n f
{-# INLINE [0] readN #-}

{-# RULES

"<$> to <*>" forall f g.
  (<$>) f g = returnG f <*> g

"readN/readN merge" forall n m f g.
  apG (readN n f) (readN m g) = readN (n+m) (\bs -> f bs $ g (B.unsafeDrop n bs))

"returnG/readN swap" [~1] forall f.
  returnG f = readN 0 (const f)

"readN 0/returnG swapback" [1] forall f.
  readN 0 f = returnG (f B.empty) #-}

-- | Ensure that there are at least @n@ bytes available. If not, the
-- computation will escape with 'Partial'.
ensureN :: Int -> Get ()
ensureN !n0 = C $ \inp ks -> do
  if B.length inp >= n0
    then ks inp ()
    else runCont (go n0) inp ks
  where -- might look a bit funny, but plays very well with GHC's inliner.
        -- GHC won't inline recursive functions, so we make ensureN non-recursive
    go n = C $ \inp ks -> do
      if B.length inp >= n
        then ks inp ()
        else runCont (demandInput >> go n) inp ks
{-# INLINE ensureN #-}

unsafeReadN :: Int -> (B.ByteString -> a) -> Get a
unsafeReadN !n f = C $ \inp ks -> do
  ks (B.unsafeDrop n inp) $! f inp -- strict return

readNWith :: Int -> (Ptr a -> IO a) -> Get a
readNWith n f = do
    readN n $ \s -> B.inlinePerformIO $ B.unsafeUseAsCString s (f . castPtr)
{-# INLINE readNWith #-}
