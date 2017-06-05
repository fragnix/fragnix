{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Text/Read/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Text.Read.Compat (
   -- * The 'Read' class
   Read(..),
   ReadS,

   -- * Haskell 2010 functions
   reads,
   read,
   readParen,
   lex,

   -- * New parsing functions
   module Text.ParserCombinators.ReadPrec,
   L.Lexeme(..),
   lexP,
   parens,
   readListDefault,
   readListPrecDefault,
   readEither,
   readMaybe

 ) where

import Text.Read
import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L

