{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "src/Data/Scientific.hs" #-}























































{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Data.Scientific
-- Copyright   :  Bas van Dijk 2013
-- License     :  BSD3
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- @Data.Scientific@ provides a space efficient and arbitrary precision
-- scientific number type.
--
-- 'Scientific' numbers are represented using
-- <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>. It
-- uses an 'Integer' 'coefficient' @c@ and an 'Int' 'base10Exponent' @e@ (do
-- note that since we're using an 'Int' to represent the exponent these numbers
-- aren't truly arbitrary precision). A scientific number corresponds to the
-- 'Fractional' number: @'fromInteger' c * 10 '^^' e@.
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
-- infinite decimal expansion.
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

import           Control.Monad                (mplus)
import           Control.DeepSeq              (NFData(rnf))
import           Data.Array                   (Array, listArray, (!))
import           Data.Char                    (intToDigit, ord)
import           Data.Data                    (Data)
import           Data.Function                (on)
import           Data.Functor                 ((<$>))
import           Data.Hashable                (Hashable(..))
import           Data.Ratio                   ((%), numerator, denominator)
import           Data.Typeable                (Typeable)
import           Math.NumberTheory.Logarithms (integerLog10')
import qualified Numeric                      (floatToDigits)
import qualified Text.Read                       as Read
import           Text.Read                        (readPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadP     ( ReadP )
import           Data.Text.Lazy.Builder.RealFloat (FPFormat(..))

import           Data.Bits                    (unsafeShiftR)


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
    { coefficient    ::                !Integer
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

instance Eq Scientific where
    (==) = (==) `on` toRational
    {-# INLINE (==) #-}

    (/=) = (/=) `on` toRational
    {-# INLINE (/=) #-}

instance Ord Scientific where
    (<) = (<) `on` toRational
    {-# INLINE (<) #-}

    (<=) = (<=) `on` toRational
    {-# INLINE (<=) #-}

    (>) = (>) `on` toRational
    {-# INLINE (>) #-}

    (>=) = (>=) `on` toRational
    {-# INLINE (>=) #-}

    compare = compare `on` toRational
    {-# INLINE compare #-}

instance Num Scientific where
    Scientific c1 e1 + Scientific c2 e2
       | e1 < e2   = scientific (c1   + c2*l) e1
       | otherwise = scientific (c1*r + c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (+) #-}

    Scientific c1 e1 - Scientific c2 e2
       | e1 < e2   = scientific (c1   - c2*l) e1
       | otherwise = scientific (c1*r - c2  ) e2
         where
           l = magnitude (e2 - e1)
           r = magnitude (e1 - e2)
    {-# INLINE (-) #-}

    Scientific c1 e1 * Scientific c2 e2 =
        scientific (c1 * c2) (e1 + e2)
    {-# INLINE (*) #-}

    abs (Scientific c e) = Scientific (abs c) e
    {-# INLINE abs #-}

    negate (Scientific c e) = Scientific (negate c) e
    {-# INLINE negate #-}

    signum (Scientific c _) = Scientific (signum c) 0
    {-# INLINE signum #-}

    fromInteger i = scientific i 0
    {-# INLINE fromInteger #-}

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
    {-# INLINE toRational #-}

{-# RULES
  "realToFrac_toRealFloat_Double"
   realToFrac = toRealFloat :: Scientific -> Double #-}

{-# RULES
  "realToFrac_toRealFloat_Float"
   realToFrac = toRealFloat :: Scientific -> Float #-}

-- | /WARNING:/ 'recip' and '/' will diverge (i.e. loop and consume all space)
-- when their outputs have an infinite decimal expansion. 'fromRational' will
-- diverge when the input 'Rational' has an infinite decimal expansion.
instance Fractional Scientific where
    recip = fromRational . recip . toRational
    {-# INLINE recip #-}

    x / y = fromRational $ toRational x / toRational y
    {-# INLINE (/) #-}

    fromRational rational = positivize (longDiv 0 0) (numerator rational)
      where
        -- Divide the numerator by the denominator using long division.
        longDiv :: Integer -> Int -> (Integer -> Scientific)
        longDiv !c !e  0 = scientific c e
        longDiv !c !e !n
                          -- TODO: Use a logarithm here!
            | n < d     = longDiv (c * 10) (e - 1) (n * 10)
            | otherwise = longDiv (c + q)   e      r
                            where
                              (q, r) = n `quotRem` d

        d = denominator rational

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
                      else let (q, r) = c `quotRem` magnitude (-e)
                           in (fromInteger q, scientific r e)
        | otherwise = (toIntegral s, 0)
    {-# INLINE properFraction #-}

    -- | @'truncate' s@ returns the integer nearest @s@
    -- between zero and @s@
    truncate = whenFloating $ \c e ->
                 if dangerouslySmall c e
                 then 0
                 else fromInteger $ c `quot` magnitude (-e)
    {-# INLINE truncate #-}

    -- | @'round' s@ returns the nearest integer to @s@;
    --   the even integer if @s@ is equidistant between two integers
    round = whenFloating $ \c e ->
              if dangerouslySmall c e
              then 0
              else let (q, r) = c `quotRem` magnitude (-e)
                       n = fromInteger q
                       m = if r < 0 then n - 1 else n + 1
                       f = scientific r e
                   in case signum $ coefficient $ abs f - 0.5 of
                        -1 -> n
                        0  -> if even n then n else m
                        1  -> m
                        _  -> error "round default defn: Bad value"
    {-# INLINE round #-}

    -- | @'ceiling' s@ returns the least integer not less than @s@
    ceiling = whenFloating $ \c e ->
                if dangerouslySmall c e
                then if c <= 0
                     then 0
                     else 1
                else let (q, r) = c `quotRem` magnitude (-e)
                     in fromInteger $! if r <= 0 then q else q + 1
    {-# INLINE ceiling #-}

    -- | @'floor' s@ returns the greatest integer not greater than @s@
    floor = whenFloating $ \c e ->
              if dangerouslySmall c e
              then if c < 0
                   then -1
                   else 0
              else fromInteger (c `div` magnitude (-e))
    {-# INLINE floor #-}


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
positivize f x | x < 0      = -(f (-x))
               | otherwise =    f   x
{-# INLINE positivize #-}

whenFloating :: (Num a) => (Integer -> Int -> a) -> Scientific -> a
whenFloating f s@(Scientific c e)
    | e < 0     = f c e
    | otherwise = toIntegral s
{-# INLINE whenFloating #-}

-- | Precondition: the 'Scientific' @s@ needs to be an integer:
-- @base10Exponent (normalize s) >= 0@
toIntegral :: (Num a) => Scientific -> a
toIntegral (Scientific c e) = fromInteger c * magnitude e
{-# INLINE toIntegral #-}


----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: Array Int Integer
expts10 = listArray (0, maxExpt) $ 1 : 10 : go 2
    where
      go :: Int -> [Integer]
      go !ix = xx : 10*xx : go (ix+2)
          where
            xx = x * x
            x  = expts10 ! half

            half = ix `unsafeShiftR` 1

-- | @magnitude e == 10 ^ e@
magnitude :: (Num a) => Int -> a
magnitude e | e <= maxExpt = cachedPow10 e
            | otherwise    = cachedPow10 maxExpt * 10 ^ (e - maxExpt)
    where
      cachedPow10 p = fromInteger (expts10 ! p)
{-# INLINE magnitude #-}


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
fromFloatDigits = positivize fromNonNegRealFloat
    where
      fromNonNegRealFloat r = go digits 0 0
        where
          (digits, e) = Numeric.floatToDigits 10 r

          go []     !c !n = Scientific c (e - n)
          go (d:ds) !c !n = go ds (c * 10 + fromIntegral d) (n + 1)

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

-- | Preciser version of `toRealFloat`. If the 'base10Exponent' of the given
-- 'Scientific' is too big or too small to be represented in the target type,
-- Infinity or 0 will be returned as 'Left'.
toBoundedRealFloat :: forall a. (RealFloat a) => Scientific -> Either a a
toBoundedRealFloat s@(Scientific c e)
    | c == 0                                       = Right 0
    | e >  limit && e > hiLimit                    = Left  $ sign (1/0) -- Infinity
    | e < -limit && e < loLimit && e + d < loLimit = Left  $ sign 0
    | otherwise                                    = Right $ realToFrac s
  where
    (loLimit, hiLimit) = exponentLimits (undefined :: a)

    d = integerLog10' (abs c)

    sign x | c < 0     = -x
           | otherwise =  x

exponentLimits :: forall a. (RealFloat a) => a -> (Int, Int)
exponentLimits _ = (loLimit, hiLimit)
    where
      loLimit = floor   (fromIntegral lo     * log10Radix) -
                ceiling (fromIntegral digits * log10Radix)
      hiLimit = ceiling (fromIntegral hi     * log10Radix)

      log10Radix :: Double
      log10Radix = logBase 10 $ fromInteger radix

      radix    = floatRadix  (undefined :: a)
      digits   = floatDigits (undefined :: a)
      (lo, hi) = floatRange  (undefined :: a)

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
                     e > integerLog10' (max (abs $ toInteger (minBound :: i))
                                            (abs $ toInteger (maxBound :: i)))

    fromIntegerBounded :: Integer -> Maybe i
    fromIntegerBounded i
        | i < toInteger (minBound :: i) ||
          i > toInteger (maxBound :: i) = Nothing
        | otherwise = Just $ fromInteger i

    -- This should not be evaluated if the given Scientific is dangerouslyBig
    -- since it could consume all space and crash the process:
    n :: Integer
    n = toIntegral s'

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
           ((scientific signedCoeff . (expnt +)) <$> eP)) `mplus`
     return (scientific signedCoeff    expnt)

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
    show = formatScientific Generic Nothing

-- | Like 'show' but provides rendering options.
formatScientific :: FPFormat
                 -> Maybe Int  -- ^ Number of decimal places to render.
                 -> Scientific
                 -> String
formatScientific fmt decs scntfc@(Scientific c _)
   | c < 0     = '-':doFmt fmt (toDecimalDigits (-scntfc))
   | otherwise =     doFmt fmt (toDecimalDigits   scntfc )
  where
    doFmt :: FPFormat -> ([Int], Int) -> String
    doFmt format (is, e) =
      let ds = map intToDigit is in
      case format of
       Generic ->
        doFmt (if e < 0 || e > 7 then Exponent else Fixed)
              (is, e)
       Exponent ->
        case decs of
         Nothing ->
          let show_e' = show (e-1) in
          case ds of
            "0"     -> "0.0e0"
            [d]     -> d : ".0e" ++ show_e'
            (d:ds') -> d : '.' : ds' ++ "e" ++ show_e'
            []      -> error "formatScientific/doFmt/FFExponent: []"
         Just dec ->
          let dec' = max dec 1 in
          case is of
           [0] -> '0' :'.' : take dec' (repeat '0') ++ "e0"
           _ ->
            let
             (ei,is') = roundTo (dec'+1) is
             (d:ds') = map intToDigit (if ei > 0 then init is' else is')
            in
            d:'.':ds' ++ 'e':show (e-1+ei)
       Fixed ->
        let
         mk0 ls = case ls of { "" -> "0" ; _ -> ls}
        in
        case decs of
         Nothing
            | e <= 0    -> "0." ++ replicate (-e) '0' ++ ds
            | otherwise ->
               let
                  f 0 s    rs  = mk0 (reverse s) ++ '.':mk0 rs
                  f n s    ""  = f (n-1) ('0':s) ""
                  f n s (r:rs) = f (n-1) (r:s) rs
               in
                  f e "" ds
         Just dec ->
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

----------------------------------------------------------------------

roundTo :: Int -> [Int] -> (Int,[Int])
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  base = 10

  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

----------------------------------------------------------------------

-- | Similar to 'Numeric.floatToDigits', @toDecimalDigits@ takes a
-- non-negative 'Scientific' number, and returns a list of digits and
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
toDecimalDigits (Scientific c' e') = (is,  n + e)
  where
    Scientific c e = normalizePositive c' e'

    (is, n) = reverseAndLength $ digits c

    digits :: Integer -> [Int]
    digits 0 = []
    digits i = fromIntegral r : digits q
      where
        (q, r) = i `quotRem` 10

    reverseAndLength :: [a] -> ([a], Int)
    reverseAndLength l = rev l [] 0
      where
        rev []     a !m = (a, m)
        rev (x:xs) a !m = rev xs (x:a) (m+1)


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
    | c < 0 = -(normalizePositive (-c) e)
    | c > 0 =   normalizePositive   c  e
    | otherwise {- c == 0 -} = Scientific 0 0

normalizePositive :: Integer -> Int -> Scientific
normalizePositive c !e = case quotRem c 10 of
                           (q, 0) -> normalizePositive q (e+1)
                           _      -> Scientific c e
