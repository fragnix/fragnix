{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Text/Appar/Parser.hs" #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This is subset of Parsec.

Parsec 3 provides features which Parsec 2 does not provide:

* Applicative style

* ByteString as input

But Haskell Platform includes Parsec 2, not Parsec 3. Installing
Parsec 3 to Haskell Platform environment makes it mess. So, this library
was implemented.
-}

module Text.Appar.Parser (
  -- ** Running parser
    parse
  -- ** 'Char' parsers
  , char
  , anyChar
  , oneOf
  , noneOf
  , alphaNum
  , digit
  , hexDigit
  , space
  -- ** 'String' parser
  , string
  -- ** Parser combinators
  , try
  , choice
  , option
  , skipMany
  , skipSome
  , sepBy1
  , manyTill
  -- ** 'Applicative' parser combinators
  , (<$>)
  , (<$)
  , (<*>)
  , (*>)
  , (<*)
  , (<**>)
  , (<|>)
  , some
  , many
  , pure
  -- ** Internals
  , MkParser(..)
  , Input(..)
  , satisfy
  ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.Appar.Input

----------------------------------------------------------------

data MkParser inp a = P {
  -- | Getting the internal parser.
    runParser :: inp -> (Maybe a, inp)
  }

----------------------------------------------------------------

instance Functor (MkParser inp) where
    f `fmap` p = return f <*> p

instance Applicative (MkParser inp) where
    pure  = return
    (<*>) = ap

instance Alternative (MkParser inp) where
    empty = mzero
    (<|>) = mplus

instance Monad (MkParser inp) where
    return a = P $ \bs -> (Just a, bs)
    p >>= f  = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> (Nothing, bs')
        (Just a,  bs') -> runParser (f a) bs'
    fail _   = P $ \bs -> (Nothing, bs)

instance MonadPlus (MkParser inp) where
    mzero       = P $ \bs -> (Nothing, bs)
    p `mplus` q = P $ \bs -> case runParser p bs of
        (Nothing, bs') -> runParser q bs'
        (Just a,  bs') -> (Just a, bs')

----------------------------------------------------------------

{-|
  Run a parser.
-}
parse :: Input inp => MkParser inp a -> inp -> Maybe a
parse p bs = fst (runParser p bs)

----------------------------------------------------------------
{-|
  The parser @satisfy f@ succeeds for any character for which the
  supplied function @f@ returns 'True'. Returns the character that is
  actually parsed.
-}
satisfy :: Input inp => (Char -> Bool) -> MkParser inp Char
satisfy predicate = P sat
  where
    sat bs
      | isNil bs    = (Nothing, nil)
      | predicate b = (Just b,  bs')
      | otherwise   = (Nothing, bs)
      where
        b = car bs
        bs' = cdr bs

----------------------------------------------------------------
{-|
  The parser try p behaves like parser p, except that it pretends
  that it hasn't consumed any input when an error occurs.
-}
try :: MkParser inp a -> MkParser inp a
try p = P $ \bs -> case runParser p bs of
        (Nothing, _  ) -> (Nothing, bs)
        (Just a,  bs') -> (Just a,  bs')

----------------------------------------------------------------

{-|
  @char c@ parses a single character @c@. Returns the parsed character.
-}
char :: Input inp => Char -> MkParser inp Char
char c = satisfy (c ==)

{-|
  @string s@ parses a sequence of characters given by @s@. Returns
  the parsed string
-}
string :: Input inp => String -> MkParser inp String
string []     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

----------------------------------------------------------------

{-|
  This parser succeeds for any character. Returns the parsed character.
-}
anyChar :: Input inp => MkParser inp Char
anyChar = satisfy (const True)

{-|
  @oneOf cs@ succeeds if the current character is in the supplied list of
  characters @cs@. Returns the parsed character.
-}
oneOf :: Input inp => String -> MkParser inp Char
oneOf cs = satisfy (`elem` cs)

{-|
  As the dual of 'oneOf', @noneOf cs@ succeeds if the current
  character /not/ in the supplied list of characters @cs@. Returns the
  parsed character.
-}
noneOf :: Input inp => String -> MkParser inp Char
noneOf cs = satisfy (`notElem` cs)

{-|
  Parses a letter or digit (a character between \'0\' and \'9\').
  Returns the parsed character.
-}
alphaNum :: Input inp => MkParser inp Char
alphaNum = satisfy isAlphaNum

{-|
  Parses a digit. Returns the parsed character.
-}
digit :: Input inp => MkParser inp Char
digit = satisfy isDigit

{-|
  Parses a hexadecimal digit (a digit or a letter between \'a\' and
  \'f\' or \'A\' and \'F\'). Returns the parsed character.
-}
hexDigit :: Input inp => MkParser inp Char
hexDigit = satisfy isHexDigit

{-|
  Parses a white space character (any character which satisfies 'isSpace')
   Returns the parsed character.
-}
space :: Input inp => MkParser inp Char
space = satisfy isSpace

----------------------------------------------------------------

{-|
  @choice ps@ tries to apply the parsers in the list @ps@ in order,
  until one of them succeeds. Returns the value of the succeeding
  parser.
-}
choice :: [MkParser inp a] -> MkParser inp a
choice = foldr (<|>) mzero

{-|
  @option x p@ tries to apply parser @p@. If @p@ fails without
  consuming input, it returns the value @x@, otherwise the value
  returned by @p@.
-}
option :: a -> MkParser inp a -> MkParser inp a
option x p = p <|> pure x

{-|
  @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
  its result.
-}
skipMany :: MkParser inp a -> MkParser inp ()
skipMany p = () <$ many p

{-|
  @skipSome p@ applies the parser @p@ /one/ or more times, skipping
  its result.
-}
skipSome :: MkParser inp a -> MkParser inp ()
skipSome p = () <$ some p

{-|
  @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
  by @sep@. Returns a list of values returned by @p@.
-}
sepBy1 :: MkParser inp a -> MkParser inp b -> MkParser inp [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

{-|
  @manyTill p end@ applies parser @p@ /zero/ or more times until
  parser @end@ succeeds. Returns the list of values returned by @p@.
-}
manyTill :: MkParser inp a -> MkParser inp b -> MkParser inp [a]
manyTill p end = scan
  where
    scan = [] <$ end <|> (:) <$> p <*> scan
