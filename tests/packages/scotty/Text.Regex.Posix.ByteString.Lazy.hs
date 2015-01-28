{-# LANGUAGE Haskell98, MultiParamTypeClasses, FunctionalDependencies, CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# LINE 1 "Text/Regex/Posix/ByteString/Lazy.hs" #-}

















































{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix.ByteString.Lazy
-- Copyright   :  (c) Chris Kuklewicz 2007
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- This provides 'ByteString.Lazy' instances for RegexMaker and RegexLike
-- based on "Text.Regex.Posix.Wrap", and a (RegexContext Regex
-- ByteString ByteString) instance.
--
-- To use these instance, you would normally import
-- "Text.Regex.Posix".  You only need to import this module to use
-- the medium level API of the compile, regexec, and execute
-- functions.  All of these report error by returning Left values
-- instead of undefined or error or fail.
--
-- A Lazy ByteString with more than one chunk cannot be be passed to
-- the library efficiently (as a pointer).  It will have to converted
-- via a full copy to a temporary normal bytestring (with a null byte
-- appended if necessary).
-----------------------------------------------------------------------------

module Text.Regex.Posix.ByteString.Lazy(
  -- ** Types
  Regex,
  MatchOffset,
  MatchLength,
  ReturnCode,
  WrapError,
  -- ** Miscellaneous
  unusedOffset,
  -- ** Medium level API functions
  compile,
  execute,
  regexec,
  -- ** Compilation options
  CompOption(CompOption),
  compBlank,
  compExtended,   -- use extended regex syntax
  compIgnoreCase, -- ignore case when matching
  compNoSub,      -- no substring matching needed
  compNewline,    -- '.' doesn't match newline
  -- ** Execution options
  ExecOption(ExecOption),
  execBlank,
  execNotBOL,      -- not at begining of line
  execNotEOL       -- not at end of line
  ) where

import Data.Array(Array)
import qualified Data.ByteString.Lazy as L (ByteString,null,toChunks,fromChunks,last,snoc)
import qualified Data.ByteString as B(ByteString,concat)
import qualified Data.ByteString.Unsafe as B(unsafeUseAsCString)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexContext(..),RegexLike(..),MatchOffset,MatchLength)
import Text.Regex.Posix.Wrap -- all
import qualified Text.Regex.Posix.ByteString as BS(execute,regexec)
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Foreign.C.String(CString)

instance RegexContext Regex L.ByteString L.ByteString where
  match = polymatch
  matchM = polymatchM

fromLazy :: L.ByteString -> B.ByteString
fromLazy = B.concat . L.toChunks

toLazy :: B.ByteString -> L.ByteString
toLazy = L.fromChunks . return

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.Posix.ByteString.Lazy died: "++ show err)
                     Right v -> return v

{-# INLINE asCString #-}
asCString :: L.ByteString -> (CString -> IO a) -> IO a
asCString s = if (not (L.null s)) && (0==L.last s)
                then B.unsafeUseAsCString (fromLazy s)
                else B.unsafeUseAsCString (fromLazy (L.snoc s 0))

instance RegexMaker Regex CompOption ExecOption L.ByteString where
  makeRegexOpts c e pattern = unsafePerformIO $ compile c e pattern >>= unwrap
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $ compile c e pattern

instance RegexLike Regex L.ByteString where
  matchTest regex bs = unsafePerformIO $
    asCString bs (wrapTest regex) >>=  unwrap
  matchOnce regex bs = unsafePerformIO $ 
    execute regex bs >>= unwrap
  matchAll regex bs = unsafePerformIO $
    asCString bs (wrapMatchAll regex) >>= unwrap
  matchCount regex bs = unsafePerformIO $
    asCString bs (wrapCount regex) >>= unwrap

-- ---------------------------------------------------------------------
-- | Compiles a regular expression
--
compile :: CompOption    -- ^ Flags (summed together)
        -> ExecOption    -- ^ Flags (summed together)
        -> L.ByteString  -- ^ The regular expression to compile
        -> IO (Either WrapError Regex)      -- ^ Returns: the compiled regular expression
compile c e pattern = asCString pattern (wrapCompile c e)

-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex bs = if (not (L.null bs)) && (0==L.last bs)
                     then BS.execute regex (fromLazy bs)
                     else BS.execute regex (fromLazy (L.snoc bs 0))

regexec :: Regex      -- ^ Compiled regular expression
        -> L.ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (L.ByteString, L.ByteString, L.ByteString, [L.ByteString])))
regexec regex bs = do
  x <- if (not (L.null bs)) && (0==L.last bs)
         then BS.regexec regex (fromLazy bs)
         else BS.regexec regex (fromLazy (L.snoc bs 0))
  return $ case x of
             Left e -> Left e
             Right Nothing -> Right Nothing
             Right (Just (a,b,c,ds)) -> Right (Just (toLazy a,toLazy b,toLazy c,map toLazy ds))

unusedOffset :: Int
unusedOffset = fromIntegral unusedRegOffset
