{-# LANGUAGE Haskell98, MultiParamTypeClasses, FunctionalDependencies, CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# LINE 1 "Text/Regex/Posix/ByteString.hs" #-}


























































{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix.ByteString
-- Copyright   :  (c) Chris Kuklewicz 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- This provides 'ByteString' instances for RegexMaker and RegexLike
-- based on "Text.Regex.Posix.Wrap", and a (RegexContext Regex
-- ByteString ByteString) instance.
--
-- To use these instance, you would normally import
-- "Text.Regex.Posix".  You only need to import this module to use
-- the medium level API of the compile, regexec, and execute
-- functions.  All of these report error by returning Left values
-- instead of undefined or error or fail.
--
-- The ByteString will only be passed to the library efficiently (as a
-- pointer) if it ends in a NUL byte.  Otherwise a temporary copy must
-- be made with the 0 byte appended.
-----------------------------------------------------------------------------

module Text.Regex.Posix.ByteString(
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

import Data.Array(Array,listArray)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B(empty,useAsCString,last,take,drop,null)
import qualified Data.ByteString.Unsafe as B(unsafeUseAsCString)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexMaker(..),RegexContext(..),RegexLike(..),MatchOffset,MatchLength)
import Text.Regex.Posix.Wrap -- all
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Foreign.C.String(CString)

instance RegexContext Regex ByteString ByteString where
  match = polymatch
  matchM = polymatchM

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.Posix.ByteString died: "++ show err)
                     Right v -> return v

instance RegexMaker Regex CompOption ExecOption ByteString where
  makeRegexOpts c e pattern = unsafePerformIO $
    compile c e pattern >>= unwrap
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $
    compile c e pattern

instance RegexLike Regex ByteString where
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
        -> ByteString  -- ^ The regular expression to compile
        -> IO (Either WrapError Regex)      -- ^ Returns: the compiled regular expression
compile c e pattern =
  asCString pattern (wrapCompile c e)


-- ---------------------------------------------------------------------
-- | Matches a regular expression against a buffer, returning the buffer
-- indicies of the match, and any submatches
--
-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.
execute regex bs = do
  maybeStartEnd <- asCString bs (wrapMatch regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> ...
    Right (Just parts) -> 
      return . Right . Just . listArray (0,pred (length parts))
      . map (\(s,e)->(fromIntegral s, fromIntegral (e-s))) $ parts
    Left err -> return (Left err)

regexec :: Regex      -- ^ Compiled regular expression
        -> ByteString -- ^ String to match against
        -> IO (Either WrapError (Maybe (ByteString, ByteString, ByteString, [ByteString])))
regexec regex bs = do
  let getSub (start,stop) | start == unusedRegOffset = B.empty
                          | otherwise = B.take (fi (stop-start)) . B.drop (fi start) $ bs
      matchedParts [] = (B.empty,B.empty,bs,[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) = 
        (B.take (fi start) bs
        ,getSub matchedStartStop
        ,B.drop (fi stop) bs
        ,map getSub subStartStop)
  maybeStartEnd <- asCString bs (wrapMatch regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) -> ...
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)

unusedOffset :: Int
unusedOffset = fromIntegral unusedRegOffset

fi :: (Integral i,Num n) => i->n
fi = fromIntegral

asCString :: ByteString -> (CString -> IO a) -> IO a
asCString bs = if (not (B.null bs)) && (0==B.last bs)
                  then B.unsafeUseAsCString bs
                  else B.useAsCString bs
