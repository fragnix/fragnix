{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "src/Data/Scientific.hs" #-}












































































{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternGuards #-}

-- |
-- Module      :  Data.Scientific
-- Copyright   :  Bas van Dijk 2013
-- License     :  BSD3
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module provides the number type 'Scientific'. Scientific numbers are
-- arbitrary precision and space efficient. They are represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>. The
-- implementation uses an 'Integer' 'coefficient' @c@ and an 'Int'
-- 'base10Exponent' @e@. A scientific number corresponds to the 'Fractional'
-- number: @'fromInteger' c * 10 '^^' e@.
--
-- Note that since we're using an 'Int' to represent the exponent these numbers
-- aren't truly arbitrary precision. I intend to change the type of the exponent
-- to 'Integer' in a future release.
--
-- The main application of 'Scientific' is to be used as the target of parsing
-- arbitrary precision numbers coming from an untrusted source. The advantages
-- over using 'Rational' for this are that:
--
-- * A 'Scientific' is more efficient to construct. Rational numbers need to be
-- constructed using '%' which has to compute the 'gcd' of the 'numerator' and
-- 'denominator'.
--
-- * 'Scientific' is safe against numbers with huge exponents. For example:
-- @1e1000000000 :: 'Rational'@ will fill up all space and crash your
-- program. Scientific works as expected:
--
--  > > read "1e1000000000" :: Scientific
--  > 1.0e1000000000
--
-- * Also, the space usage of converting scientific numbers with huge exponents
-- to @'Integral's@ (like: 'Int') or @'RealFloat's@ (like: 'Double' or 'Float')
-- will always be bounded by the target type.
--
-- /WARNING:/ Although @Scientific@ is an instance of 'Fractional', the methods
-- are only partially defined! Specifically 'recip' and '/' will diverge
-- (i.e. loop and consume all space) when their outputs have an infinite decimal
-- expansion. 'fromRational' will diverge when the input 'Rational' has an
-- infinite decimal expansion. Consider using 'fromRationalRepetend' for these
-- rationals which will detect the repetition and indicate where it starts.
--
-- This module is designed to be imported qualified:
--
-- @import Data.Scientific as Scientific@
module Data.Scientific
    ( Scientific

      -- * Construction
    , scientific

      -- * Projections
    , coefficient
    , base10Exponent

      -- * Predicates
    , isFloating
    , isInteger

      -- * Conversions
    , fromRationalRepetend
    , toRationalRepetend
    , floatingOrInteger
    , toRealFloat
    , toBoundedRealFloat
    , toBoundedInteger
    , fromFloatDigits

      -- * Pretty printing
    , formatScientific
    , FPFormat(..)

    , toDecimalDigits

      -- * Normalization
    , normalize
    ) where


----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Exception            (throw, ArithException(DivideByZero))
import           Control.Monad                (mplus)
import           Control.Monad.ST             (runST)
import           Control.DeepSeq              (NFData, rnf)
import           Data.Binary                  (Binary, get, put)
import           Data.Char                    (intToDigit, ord)
import           Data.Data                    (Data)
import           Data.Function                (on)
import           Data.Hashable                (Hashable(..))
import           Data.Int                     (Int8, Int16, Int32, Int64)
import qualified Data.Map            as M     (Map, empty, insert, lookup)
import           Data.Ratio                   ((%), numerator, denominator)
import           Data.Typeable                (Typeable)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           Data.Word                    (Word8, Word16, Word32, Word64)
import           Math.NumberTheory.Logarithms (integerLog10')
import qualified Numeric                      (floatToDigits)
import qualified Text.Read                       as Read
import           Text.Read                        (readPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP     ( ReadP )
import           Data.Text.Lazy.Builder.RealFloat (FPFormat(..))


import           Data.Bits                    (unsafeShiftR)

import GHC.Integer        (quotRemInteger, quotInteger)
import GHC.Integer.Compat (divInteger)
import Utils              (roundTo)


----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | An arbitrary-precision number represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.
--
-- This type describes the set of all @'Real's@ which have a finite
-- decimal expansion.
--
-- A scientific number with 'coefficient' @c@ and 'base10Exponent' @e@
-- corresponds to the 'Fractional' number: @'fromInteger' c * 10 '^^' e@
data Scientific = Scientific
    { coefficient :: !Integer
      -- ^ The coefficient of a scientific number.
      --
      -- Note that this number is not necessarily normalized, i.e.
      -- it could contain trailing zeros.
      --
      -- Scientific numbers are automatically normalized when pretty printed or
      -- in 'toDecimalDigits'.
      --
      -- Use 'normalize' to do manual normalization.

    , base10Exponent :: {-# UNPACK #-} !Int
      -- ^ The base-10 exponent of a scientific number.
    } deriving (Typeable, Data)

-- | @scientific c e@ constructs a scientific number which corresponds
-- to the 'Fractional' number: @'fromInteger' c * 10 '^^' e@.
scientific :: Integer -> Int -> Scientific
scientific = Scientific


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance NFData Scientific where
    rnf (Scientific _ _) = ()

instance Hashable Scientific where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Binary Scientific where
    put (Scientific c e) = do
      put c
      -- In the future I intend to change the type of the base10Exponent e from
      -- Int to Integer. To support backward compatability I already convert e
      -- to Integer here:
      put $ toInteger e

    get = Scientific <$> get <*> (fromInteger <$> get)

instance Eq Scientific where
    (==) = (==) `on` toRational
    {-# INLINABLE (==) #-}

    (/=) = (/=) `on` toRational
    {-# INLINABLE (/=) #-}

instance Ord Scientific where
    (<) = (<) `on` toRational
    {-# INLINABLE (<) #-}

    (<=) = (<=) `on` toRational
    {-# INLINABLE (<=) #-}

    (>) = (>) `on` toRational
    {-# INLINABLE (>) #-}

    (>=) = (>=) `on` toRational
    {-# INLINABLE (>=) #-}

    compare = compare `on` toRational
    {-# INLINABLE compare #-}

instance Num Scientific where
    Scientific c1 e1 + Scientific c2 e2
       | e1 < e2   = Scientific (c1   + c2*l) e1
       | otherwise = Scientific (c1*r + c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINABLE (+) #-}

    Scientific c1 e1 - Scientific c2 e2
       | e1 < e2   = Scientific (c1   - c2*l) e1
       | otherwise = Scientific (c1*r - c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINABLE (-) #-}

    Scientific c1 e1 * Scientific c2 e2 =
        Scientific (c1 * c2) (e1 + e2)
    {-# INLINABLE (*) #-}

    abs (Scientific c e) = Scientific (abs c) e
    {-# INLINABLE abs #-}

    negate (Scientific c e) = Scientific (negate c) e
    {-# INLINABLE negate #-}

    signum (Scientific c _) = Scientific (signum c) 0
    {-# INLINABLE signum #-}

    fromInteger i = Scientific i 0
    {-# INLINABLE fromInteger #-}

-- | /WARNING:/ 'toRational' needs to compute the 'Integer' magnitude:
-- @10^e@. If applied to a huge exponent this could fill up all space
-- and crash your program!
--
-- Avoid applying 'toRational' (or 'realToFrac') to scientific numbers
-- coming from an untrusted source and use 'toRealFloat' instead. The
-- latter guards against excessive space usage.
instance Real Scientific where
    toRational (Scientific c e)
      | e < 0     =  c % magnitude (-e)
      | otherwise = (c * magnitude   e) % 1
    {-# INLINABLE toRational #-}

{-# RULES
  "realToFrac_toRealFloat_Double"
   realToFrac = toRealFloat :: Scientific -> Double #-}

{-# RULES
  "realToFrac_toRealFloat_Float"
   realToFrac = toRealFloat :: Scientific -> Float #-}

-- | /WARNING:/ 'recip' and '/' will diverge (i.e. loop and consume all space)
-- when their outputs are <https://en.wikipedia.org/wiki/Repeating_decimal repeating decimals>.
--
-- 'fromRational' will diverge when the input 'Rational' is a repeating decimal.
-- Consider using 'fromRationalRepetend' for these rationals which will detect
-- the repetition and indicate where it starts.
instance Fractional Scientific where
    recip = fromRational . recip . toRational
    {-# INLINABLE recip #-}

    x / y = fromRational $ toRational x / toRational y
    {-# INLINABLE (/) #-}

    fromRational rational
        | d == 0    = throw DivideByZero
        | otherwise = positivize (longDiv 0 0) (numerator rational)
      where
        -- Divide the numerator by the denominator using long division.
        longDiv :: Integer -> Int -> (Integer -> Scientific)
        longDiv !c !e  0 = Scientific c e
        longDiv !c !e !n
                          -- TODO: Use a logarithm here!
            | n < d     = longDiv (c * 10) (e - 1) (n * 10)
            | otherwise = case n `quotRemInteger` d of
                            (#q, r#) -> longDiv (c + q) e r

        d = denominator rational

-- | Like 'fromRational', this function converts a `Rational` to a `Scientific`
-- but instead of diverging (i.e loop and consume all space) on
-- <https://en.wikipedia.org/wiki/Repeating_decimal repeating decimals>
-- it detects the repeating part, the /repetend/, and returns where it starts.
--
-- To detect the repetition this function consumes space linear in the number of
-- digits in the resulting scientific. In order to bound the space usage an
-- optional limit can be specified. If the number of digits reaches this limit
-- @Left (s, r)@ will be returned. Here @s@ is the 'Scientific' constructed so
-- far and @r@ is the remaining 'Rational'. @toRational s + r@ yields the
-- original 'Rational'
--
-- If the limit is not reached or no limit was specified @Right (s,
-- mbRepetendIx)@ will be returned. Here @s@ is the 'Scientific' without any
-- repetition and @mbRepetendIx@ specifies if and where in the fractional part
-- the repetend begins.
--
-- For example:
--
-- @fromRationalRepetend Nothing (1 % 28) == Right (3.571428e-2, Just 2)@
--
-- This represents the repeating decimal: @0.03571428571428571428...@
-- which is sometimes also unambiguously denoted as @0.03(571428)@.
-- Here the repetend is enclosed in parentheses and starts at the 3rd digit (index 2)
-- in the fractional part. Specifying a limit results in the following:
--
-- @fromRationalRepetend (Just 4) (1 % 28) == Left (3.5e-2, 1 % 1400)@
--
-- You can expect the following property to hold.
--
-- @ forall (mbLimit :: Maybe Int) (r :: Rational).
-- r == (case 'fromRationalRepetend' mbLimit r of
--        Left (s, r') -> toRational s + r'
--        Right (s, mbRepetendIx) ->
--          case mbRepetendIx of
--            Nothing         -> toRational s
--            Just repetendIx -> 'toRationalRepetend' s repetendIx)
-- @
fromRationalRepetend
    :: Maybe Int -- ^ Optional limit
    -> Rational
    -> Either (Scientific, Rational)
              (Scientific, Maybe Int)
fromRationalRepetend mbLimit rational
        | d == 0    = throw DivideByZero
        | num < 0   = case longDiv (-num) of
                        Left  (s, r)  -> Left  (-s, -r)
                        Right (s, mb) -> Right (-s, mb)
        | otherwise = longDiv num
      where
        num = numerator rational

        longDiv :: Integer -> Either (Scientific, Rational) (Scientific, Maybe Int)
        longDiv n = case mbLimit of
                      Nothing -> Right $ longDivNoLimit 0 0 M.empty n
                      Just l  -> longDivWithLimit (-l) n

        -- Divide the numerator by the denominator using long division.
        longDivNoLimit :: Integer
                       -> Int
                       -> M.Map Integer Int
                       -> (Integer -> (Scientific, Maybe Int))
        longDivNoLimit !c !e _ns 0 = (Scientific c e, Nothing)
        longDivNoLimit !c !e  ns !n
            | Just e' <- M.lookup n ns = (Scientific c e, Just (-e'))
            | n < d     = let !ns' = M.insert n e ns
                          in longDivNoLimit (c * 10) (e - 1) ns' (n * 10)
            | otherwise = case n `quotRemInteger` d of
                            (#q, r#) -> longDivNoLimit (c + q) e ns r

        longDivWithLimit :: Int -> Integer -> Either (Scientific, Rational) (Scientific, Maybe Int)
        longDivWithLimit l = go 0 0 M.empty
            where
              go :: Integer
                 -> Int
                 -> M.Map Integer Int
                 -> (Integer -> Either (Scientific, Rational) (Scientific, Maybe Int))
              go !c !e _ns 0 = Right (Scientific c e, Nothing)
              go !c !e  ns !n
                  | Just e' <- M.lookup n ns = Right (Scientific c e, Just (-e'))
                  | e <= l    = Left (Scientific c e, n % (d * magnitude (-e)))
                  | n < d     = let !ns' = M.insert n e ns
                                in go (c * 10) (e - 1) ns' (n * 10)
                  | otherwise = case n `quotRemInteger` d of
                                  (#q, r#) -> go (c + q) e ns r

        d = denominator rational

-- |
-- Converts a `Scientific` with a /repetend/ (a repeating part in the fraction),
-- which starts at the given index, into its corresponding 'Rational'.
--
-- For example to convert the repeating decimal @0.03(571428)@ you would use:
-- @toRationalRepetend 0.03571428 2 == 1 % 28@
--
-- Preconditions for @toRationalRepetend s r@:
--
-- * @r >= 0@
--
-- * @r < -(base10Exponent s)@
--
-- The formula to convert the @Scientific@ @s@
-- with a repetend starting at index @r@ is described in the paper:
-- <http://fiziko.bureau42.com/teaching_tidbits/turning_repeating_decimals_into_fractions.pdf turning_repeating_decimals_into_fractions.pdf>
-- and is defined as follows:
--
-- @
--   (fromInteger nonRepetend + repetend % nines) /
--   fromInteger (10^^r)
-- where
--   c  = coefficient s
--   e  = base10Exponent s
--
--   -- Size of the fractional part.
--   f = (-e)
--
--   -- Size of the repetend.
--   n = f - r
--
--   m = 10^^n
--
--   (nonRepetend, repetend) = c \`quotRem\` m
--
--   nines = m - 1
-- @
-- Also see: 'fromRationalRepetend'.
toRationalRepetend
    :: Scientific
    -> Int -- ^ Repetend index
    -> Rational
toRationalRepetend s r
    | r < 0  = error "toRationalRepetend: Negative repetend index!"
    | r >= f = error "toRationalRepetend: Repetend index >= than number of digits in the fractional part!"
    | otherwise = (fromInteger nonRepetend + repetend % nines) /
                  fromInteger (magnitude r)
  where
    c  = coefficient s
    e  = base10Exponent s

    -- Size of the fractional part.
    f = (-e)

    -- Size of the repetend.
    n = f - r

    m = magnitude n

    (#nonRepetend, repetend#) = c `quotRemInteger` m

    nines = m - 1

instance RealFrac Scientific where
    -- | The function 'properFraction' takes a Scientific number @s@
    -- and returns a pair @(n,f)@ such that @s = n+f@, and:
    --
    -- * @n@ is an integral number with the same sign as @s@; and
    --
    -- * @f@ is a fraction with the same type and sign as @s@,
    --   and with absolute value less than @1@.
    properFraction s@(Scientific c e)
        | e < 0     = if dangerouslySmall c e
                      then (0, s)
                      else case c `quotRemInteger` magnitude (-e) of
                             (#q, r#) -> (fromInteger q, Scientific r e)
        | otherwise = (toIntegral s, 0)
    {-# INLINABLE properFraction #-}

    -- | @'truncate' s@ returns the integer nearest @s@
    -- between zero and @s@
    truncate = whenFloating $ \c e ->
                 if dangerouslySmall c e
                 then 0
                 else fromInteger $ c `quotInteger` magnitude (-e)
    {-# INLINABLE truncate #-}

    -- | @'round' s@ returns the nearest integer to @s@;
    --   the even integer if @s@ is equidistant between two integers
    round = whenFloating $ \c e ->
              if dangerouslySmall c e
              then 0
              else let (#q, r#) = c `quotRemInteger` magnitude (-e)
                       n = fromInteger q
                       m | r < 0     = n - 1
                         | otherwise = n + 1
                       f = Scientific r e
                   in case signum $ coefficient $ abs f - 0.5 of
                        -1 -> n
                        0  -> if even n then n else m
                        1  -> m
                        _  -> error "round default defn: Bad value"
    {-# INLINABLE round #-}

    -- | @'ceiling' s@ returns the least integer not less than @s@
    ceiling = whenFloating $ \c e ->
                if dangerouslySmall c e
                then if c <= 0
                     then 0
                     else 1
                else case c `quotRemInteger` magnitude (-e) of
                       (#q, r#) | r <= 0    -> fromInteger q
                                | otherwise -> fromInteger (q + 1)
    {-# INLINABLE ceiling #-}

    -- | @'floor' s@ returns the greatest integer not greater than @s@
    floor = whenFloating $ \c e ->
              if dangerouslySmall c e
              then if c < 0
                   then -1
                   else 0
              else fromInteger (c `divInteger` magnitude (-e))
    {-# INLINABLE floor #-}


----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

-- | This function is used in the 'RealFrac' methods to guard against
-- computing a huge magnitude (-e) which could take up all space.
--
-- Think about parsing a scientific number from an untrusted
-- string. An attacker could supply 1e-1000000000. Lets say we want to
-- 'floor' that number to an 'Int'. When we naively try to floor it
-- using:
--
-- @
-- floor = whenFloating $ \c e ->
--           fromInteger (c `div` magnitude (-e))
-- @
--
-- We will compute the huge Integer: @magnitude 1000000000@. This
-- computation will quickly fill up all space and crash the program.
--
-- Note that for large /positive/ exponents there is no risk of a
-- space-leak since 'whenFloating' will compute:
--
-- @fromInteger c * magnitude e :: a@
--
-- where @a@ is the target type (Int in this example). So here the
-- space usage is bounded by the target type.
--
-- For large negative exponents we check if the exponent is smaller
-- than some limit (currently -324). In that case we know that the
-- scientific number is really small (unless the coefficient has many
-- digits) so we can immediately return -1 for negative scientific
-- numbers or 0 for positive numbers.
--
-- More precisely if @dangerouslySmall c e@ returns 'True' the
-- scientific number @s@ is guaranteed to be between:
-- @-0.1 > s < 0.1@.
--
-- Note that we avoid computing the number of decimal digits in c
-- (log10 c) if the exponent is not below the limit.
dangerouslySmall :: Integer -> Int -> Bool
dangerouslySmall c e = e < (-limit) && e < (-integerLog10' (abs c)) - 1
{-# INLINE dangerouslySmall #-}

limit :: Int
limit = maxExpt

positivize :: (Ord a, Num a, Num b) => (a -> b) -> (a -> b)
positivize f x | x < 0     = -(f (-x))
               | otherwise =   f   x
{-# INLINE positivize #-}

whenFloating :: (Num a) => (Integer -> Int -> a) -> Scientific -> a
whenFloating f s@(Scientific c e)
    | e < 0     = f c e
    | otherwise = toIntegral s
{-# INLINE whenFloating #-}

-- | Precondition: the 'Scientific' @s@ needs to be an integer:
-- @base10Exponent (normalize s) >= 0@
toIntegral :: (Num a) => Scientific -> a
toIntegral (Scientific c e) = fromInteger c * fromInteger (magnitude e)
{-# INLINE toIntegral #-}


----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: V.Vector Integer
expts10 = runST $ do
    mv <- VM.unsafeNew maxExpt
    VM.unsafeWrite mv 0  1
    VM.unsafeWrite mv 1 10
    let go !ix
          | ix == maxExpt = V.unsafeFreeze mv
          | otherwise = do
              VM.unsafeWrite mv  ix        xx
              VM.unsafeWrite mv (ix+1) (10*xx)
              go (ix+2)
          where
            xx = x * x
            x  = V.unsafeIndex expts10 half
            !half = ix `unsafeShiftR` 1
    go 2

-- | @magnitude e == 10 ^ e@
magnitude :: Int -> Integer
magnitude e | e < maxExpt = cachedPow10 e
            | otherwise   = cachedPow10 hi * 10 ^ (e - hi)
    where
      cachedPow10 = V.unsafeIndex expts10

      hi = maxExpt - 1


----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

-- | Convert a 'RealFloat' (like a 'Double' or 'Float') into a 'Scientific'
-- number.
--
-- Note that this function uses 'Numeric.floatToDigits' to compute the digits
-- and exponent of the 'RealFloat' number. Be aware that the algorithm used in
-- 'Numeric.floatToDigits' doesn't work as expected for some numbers, e.g. as
-- the 'Double' @1e23@ is converted to @9.9999999999999991611392e22@, and that
-- value is shown as @9.999999999999999e22@ rather than the shorter @1e23@; the
-- algorithm doesn't take the rounding direction for values exactly half-way
-- between two adjacent representable values into account, so if you have a
-- value with a short decimal representation exactly half-way between two
-- adjacent representable values, like @5^23*2^e@ for @e@ close to 23, the
-- algorithm doesn't know in which direction the short decimal representation
-- would be rounded and computes more digits
fromFloatDigits :: (RealFloat a) => a -> Scientific
fromFloatDigits 0  = 0
fromFloatDigits rf = positivize fromPositiveRealFloat rf
    where
      fromPositiveRealFloat r = go digits 0 0
        where
          (digits, e) = Numeric.floatToDigits 10 r

          go :: [Int] -> Integer -> Int -> Scientific
          go []     !c !n = Scientific c (e - n)
          go (d:ds) !c !n = go ds (c * 10 + toInteger d) (n + 1)

{-# INLINABLE fromFloatDigits #-}

{-# SPECIALIZE fromFloatDigits :: Double -> Scientific #-}
{-# SPECIALIZE fromFloatDigits :: Float  -> Scientific #-}

-- | Safely convert a 'Scientific' number into a 'RealFloat' (like a 'Double' or a
-- 'Float').
--
-- Note that this function uses 'realToFrac' (@'fromRational' . 'toRational'@)
-- internally but it guards against computing huge Integer magnitudes (@10^e@)
-- that could fill up all space and crash your program. If the 'base10Exponent'
-- of the given 'Scientific' is too big or too small to be represented in the
-- target type, Infinity or 0 will be returned respectively. Use
-- 'toBoundedRealFloat' which explicitly handles this case by returning 'Left'.
--
-- Always prefer 'toRealFloat' over 'realToFrac' when converting from scientific
-- numbers coming from an untrusted source.
toRealFloat :: (RealFloat a) => Scientific -> a
toRealFloat = either id id . toBoundedRealFloat

{-# INLINABLE toRealFloat #-}
{-# INLINABLE toBoundedRealFloat #-}

{-# SPECIALIZE toRealFloat        :: Scientific -> Double #-}
{-# SPECIALIZE toRealFloat        :: Scientific -> Float  #-}
{-# SPECIALIZE toBoundedRealFloat :: Scientific -> Either Double Double #-}
{-# SPECIALIZE toBoundedRealFloat :: Scientific -> Either Float  Float  #-}

-- | Preciser version of `toRealFloat`. If the 'base10Exponent' of the given
-- 'Scientific' is too big or too small to be represented in the target type,
-- Infinity or 0 will be returned as 'Left'.
toBoundedRealFloat :: forall a. (RealFloat a) => Scientific -> Either a a
toBoundedRealFloat s@(Scientific c e)
    | c == 0     = Right 0
    | e >  limit = if e > hiLimit then Left $ sign (1/0) -- Infinity
                   else Right $ fromRational ((c * magnitude e) % 1)
    | e < -limit = if e < loLimit && e + d < loLimit then Left $ sign 0
                   else Right $ fromRational (c % magnitude (-e))
    | otherwise = Right $ fromRational (toRational s)
                       -- We can't use realToFrac here
                       -- because that will cause an infinite loop
                       -- when the function is specialized for Double and Float
                       -- caused by the realToFrac_toRealFloat_Double/Float rewrite RULEs.
  where
    hiLimit, loLimit :: Int
    hiLimit = ceiling (fromIntegral hi     * log10Radix)
    loLimit = floor   (fromIntegral lo     * log10Radix) -
              ceiling (fromIntegral digits * log10Radix)

    log10Radix :: Double
    log10Radix = logBase 10 $ fromInteger radix

    radix    = floatRadix  (undefined :: a)
    digits   = floatDigits (undefined :: a)
    (lo, hi) = floatRange  (undefined :: a)

    d = integerLog10' (abs c)

    sign x | c < 0     = -x
           | otherwise =  x

-- | Convert a `Scientific` to a bounded integer.
--
-- If the given `Scientific` doesn't fit in the target representation, it will
-- return `Nothing`.
--
-- This function also guards against computing huge Integer magnitudes (@10^e@)
-- that could fill up all space and crash your program.
toBoundedInteger :: forall i. (Integral i, Bounded i) => Scientific -> Maybe i
toBoundedInteger s
    | c == 0    = fromIntegerBounded 0
    | integral  = if dangerouslyBig
                  then Nothing
                  else fromIntegerBounded n
    | otherwise = Nothing
  where
    c = coefficient s

    integral = e >= 0 || e' >= 0

    e  = base10Exponent s
    e' = base10Exponent s'

    s' = normalize s

    dangerouslyBig = e > limit &&
                     e > integerLog10' (max (abs iMinBound) (abs iMaxBound))

    fromIntegerBounded :: Integer -> Maybe i
    fromIntegerBounded i
        | i < iMinBound || i > iMaxBound = Nothing
        | otherwise                      = Just $ fromInteger i

    iMinBound = toInteger (minBound :: i)
    iMaxBound = toInteger (maxBound :: i)

    -- This should not be evaluated if the given Scientific is dangerouslyBig
    -- since it could consume all space and crash the process:
    n :: Integer
    n = toIntegral s'

{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Int #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Int8 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Int16 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Int32 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Int64 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Word #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Word8 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Word16 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Word32 #-}
{-# SPECIALIZE toBoundedInteger :: Scientific -> Maybe Word64 #-}

-- | @floatingOrInteger@ determines if the scientific is floating point
-- or integer. In case it's floating-point the scientific is converted
-- to the desired 'RealFloat' using 'toRealFloat'.
--
-- Also see: 'isFloating' or 'isInteger'.
floatingOrInteger :: (RealFloat r, Integral i) => Scientific -> Either r i
floatingOrInteger s
    | base10Exponent s  >= 0 = Right (toIntegral   s)
    | base10Exponent s' >= 0 = Right (toIntegral   s')
    | otherwise              = Left  (toRealFloat  s')
  where
    s' = normalize s


----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

-- | Return 'True' if the scientific is a floating point, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isFloating :: Scientific -> Bool
isFloating = not . isInteger

-- | Return 'True' if the scientific is an integer, 'False' otherwise.
--
-- Also see: 'floatingOrInteger'.
isInteger :: Scientific -> Bool
isInteger s = base10Exponent s  >= 0 ||
              base10Exponent s' >= 0
  where
    s' = normalize s


----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

instance Read Scientific where
    readPrec = Read.parens $ ReadPrec.lift (ReadP.skipSpaces >> scientificP)

-- A strict pair
data SP = SP !Integer {-# UNPACK #-}!Int

scientificP :: ReadP Scientific
scientificP = do
  let positive = (('+' ==) <$> ReadP.satisfy isSign) `mplus` return True
  pos <- positive

  let step :: Num a => a -> Int -> a
      step a digit = a * 10 + fromIntegral digit
      {-# INLINE step #-}

  n <- foldDigits step 0

  let s = SP n 0
      fractional = foldDigits (\(SP a e) digit ->
                                 SP (step a digit) (e-1)) s

  SP coeff expnt <- (ReadP.satisfy (== '.') >> fractional)
                    ReadP.<++ return s

  let signedCoeff | pos       =   coeff
                  | otherwise = (-coeff)

      eP = do posE <- positive
              e <- foldDigits step 0
              if posE
                then return   e
                else return (-e)

  (ReadP.satisfy isE >>
           ((Scientific signedCoeff . (expnt +)) <$> eP)) `mplus`
     return (Scientific signedCoeff    expnt)


foldDigits :: (a -> Int -> a) -> a -> ReadP a
foldDigits f z = do
    c <- ReadP.satisfy isDecimal
    let digit = ord c - 48
        a = f z digit

    ReadP.look >>= go a
  where
    go !a [] = return a
    go !a (c:cs)
        | isDecimal c = do
            _ <- ReadP.get
            let digit = ord c - 48
            go (f a digit) cs
        | otherwise = return a

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

isSign :: Char -> Bool
isSign c = c == '-' || c == '+'
{-# INLINE isSign #-}

isE :: Char -> Bool
isE c = c == 'e' || c == 'E'
{-# INLINE isE #-}


----------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------

instance Show Scientific where
    show s | coefficient s < 0 = '-':showPositive (-s)
           | otherwise         =     showPositive   s
      where
        showPositive :: Scientific -> String
        showPositive = fmtAsGeneric . toDecimalDigits

        fmtAsGeneric :: ([Int], Int) -> String
        fmtAsGeneric x@(_is, e)
            | e < 0 || e > 7 = fmtAsExponent x
            | otherwise      = fmtAsFixed    x

fmtAsExponent :: ([Int], Int) -> String
fmtAsExponent (is, e) =
    case ds of
      "0"     -> "0.0e0"
      [d]     -> d : '.' :'0' : 'e' : show_e'
      (d:ds') -> d : '.' : ds' ++ ('e' : show_e')
      []      -> error "formatScientific/doFmt/FFExponent: []"
  where
    show_e' = show (e-1)

    ds = map intToDigit is

fmtAsFixed :: ([Int], Int) -> String
fmtAsFixed (is, e)
    | e <= 0    = '0':'.':(replicate (-e) '0' ++ ds)
    | otherwise =
        let
           f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
           f n s    ""  = f (n-1) ('0':s) ""
           f n s (r:rs) = f (n-1) (r:s) rs
        in
           f e "" ds
  where
    mk0 "" = "0"
    mk0 ls = ls

    ds = map intToDigit is

-- | Like 'show' but provides rendering options.
formatScientific :: FPFormat
                 -> Maybe Int  -- ^ Number of decimal places to render.
                 -> Scientific
                 -> String
formatScientific format mbDecs s
    | coefficient s < 0 = '-':formatPositiveScientific (-s)
    | otherwise         =     formatPositiveScientific   s
  where
    formatPositiveScientific :: Scientific -> String
    formatPositiveScientific s' = case format of
        Generic  -> fmtAsGeneric        $ toDecimalDigits s'
        Exponent -> fmtAsExponentMbDecs $ toDecimalDigits s'
        Fixed    -> fmtAsFixedMbDecs    $ toDecimalDigits s'

    fmtAsGeneric :: ([Int], Int) -> String
    fmtAsGeneric x@(_is, e)
        | e < 0 || e > 7 = fmtAsExponentMbDecs x
        | otherwise      = fmtAsFixedMbDecs x

    fmtAsExponentMbDecs :: ([Int], Int) -> String
    fmtAsExponentMbDecs x = case mbDecs of
                              Nothing  -> fmtAsExponent x
                              Just dec -> fmtAsExponentDecs dec x

    fmtAsFixedMbDecs :: ([Int], Int) -> String
    fmtAsFixedMbDecs x = case mbDecs of
                           Nothing  -> fmtAsFixed x
                           Just dec -> fmtAsFixedDecs dec x

    fmtAsExponentDecs :: Int -> ([Int], Int) -> String
    fmtAsExponentDecs dec (is, e) =
        let dec' = max dec 1 in
            case is of
             [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
             _ ->
              let
               (ei,is') = roundTo (dec'+1) is
               (d:ds') = map intToDigit (if ei > 0 then init is' else is')
              in
              d:'.':ds' ++ 'e':show (e-1+ei)

    fmtAsFixedDecs :: Int -> ([Int], Int) -> String
    fmtAsFixedDecs dec (is, e) =
        let dec' = max dec 0 in
        if e >= 0 then
         let
          (ei,is') = roundTo (dec' + e) is
          (ls,rs)  = splitAt (e+ei) (map intToDigit is')
         in
         mk0 ls ++ (if null rs then "" else '.':rs)
        else
         let
          (ei,is') = roundTo dec' (replicate (-e) 0 ++ is)
          d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
         in
         d : (if null ds' then "" else '.':ds')
      where
        mk0 ls = case ls of { "" -> "0" ; _ -> ls}

----------------------------------------------------------------------

-- | Similar to 'Numeric.floatToDigits', @toDecimalDigits@ takes a
-- positive 'Scientific' number, and returns a list of digits and
-- a base-10 exponent. In particular, if @x>=0@, and
--
-- > toDecimalDigits x = ([d1,d2,...,dn], e)
--
-- then
--
--     1. @n >= 1@
--     2. @x = 0.d1d2...dn * (10^^e)@
--     3. @0 <= di <= 9@
--     4. @null $ takeWhile (==0) $ reverse [d1,d2,...,dn]@
--
-- The last property means that the coefficient will be normalized, i.e. doesn't
-- contain trailing zeros.
toDecimalDigits :: Scientific -> ([Int], Int)
toDecimalDigits (Scientific 0  _)  = ([0], 0)
toDecimalDigits (Scientific c' e') =
    case normalizePositive c' e' of
      Scientific c e -> go c 0 []
        where
          go :: Integer -> Int -> [Int] -> ([Int], Int)
          go 0 !n ds = (ds, ne) where !ne = n + e
          go i !n ds = case i `quotRemInteger` 10 of
                         (# q, r #) -> go q (n+1) (d:ds)
                           where
                             !d = fromIntegral r


----------------------------------------------------------------------
-- Normalization
----------------------------------------------------------------------

-- | Normalize a scientific number by dividing out powers of 10 from the
-- 'coefficient' and incrementing the 'base10Exponent' each time.
--
-- You should rarely have a need for this function since scientific numbers are
-- automatically normalized when pretty-printed and in 'toDecimalDigits'.
normalize :: Scientific -> Scientific
normalize (Scientific c e)
    | c > 0 =   normalizePositive   c  e
    | c < 0 = -(normalizePositive (-c) e)
    | otherwise {- c == 0 -} = Scientific 0 0

normalizePositive :: Integer -> Int -> Scientific
normalizePositive !c !e = case quotRemInteger c 10 of
                            (# c', r #)
                                | r == 0    -> normalizePositive c' (e+1)
                                | otherwise -> Scientific c e
