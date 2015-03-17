{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Text/Appar/String.hs" #-}
{-|
Simple 'Applicative' parser whose input is 'String'.
The usage is the same as parsec.

Parsec 3 provides features which Parsec 2 does not provide:

* 'Applicative' style

* 'ByteString' as input

But Haskell Platform includes Parsec 2, not Parsec 3. Installing
Parsec 3 to Haskell Platform environment makes it mess. So, this library
was implemented.

-}

module Text.Appar.String (
  -- * Documentation
  -- ** Parser type
    Parser
  , module Text.Appar.Parser
  ) where

import Text.Appar.Parser

{-|
  Parser synonym for 'String'.
-}
type Parser = MkParser String
