{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Text/Appar/ByteString.hs" #-}
{-|
Simple 'Applicative' parser whose input is strict 'ByteString'.
The usage is the same as parsec.

Parsec 3 provides features which Parsec 2 does not provide:

* 'Applicative' style

* 'ByteString' as input

But Haskell Platform includes Parsec 2, not Parsec 3. Installing
Parsec 3 to Haskell Platform environment makes it mess. So, this library
was implemented.

-}

module Text.Appar.ByteString (
  -- * Documentation
  -- ** Parser type
    Parser
  , module Text.Appar.Parser
  ) where

import Data.ByteString.Char8 (ByteString)
import Text.Appar.Parser

{-|
  Parser synonym for strict 'ByteString'.
-}
type Parser = MkParser ByteString
