{-# LINE 1 "Text/Regex/Posix/Sequence.hs" #-}
# 1 "Text/Regex/Posix/Sequence.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1











































































































# 9 "<command-line>" 2
# 1 "Text/Regex/Posix/Sequence.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix.Sequence
-- Copyright   :  (c) Chris Kuklewicz 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- This provides 'String' instances for 'RegexMaker' and 'RegexLike' based
-- on "Text.Regex.Posix.Wrap", and a ('RegexContext' 'Regex' 'String' 'String')
-- instance.
--
-- To use these instance, you would normally import
-- "Text.Regex.Posix".  You only need to import this module to use
-- the medium level API of the compile, regexec, and execute
-- functions.  All of these report error by returning Left values
-- instead of undefined or error or fail.
--
-----------------------------------------------------------------------------

module Text.Regex.Posix.Sequence(
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
  regexec,
  execute,
  -- ** Compilation options
  CompOption(CompOption),
  compBlank,
  compExtended,   -- use extended regex syntax
  compIgnoreCase, -- ignore case when matching
  compNoSub,      -- no substring matching needed
  compNewline,    -- '.' doesn't match newline

  ExecOption(ExecOption),
  execBlank,
  execNotBOL,     -- not at begining of line
  execNotEOL     -- not at end of line
  ) where

import Data.Array(listArray, Array)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexContext(..),RegexMaker(..),RegexLike(..),MatchOffset,MatchLength,Extract(..))
import Text.Regex.Posix.Wrap
import Text.Regex.Base.Impl(polymatch,polymatchM)
import Data.Sequence as S hiding (length)
import qualified Data.Sequence as S (length)
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

instance RegexContext Regex (Seq Char) (Seq Char) where
  match = polymatch
  matchM = polymatchM

unusedOffset :: Int
unusedOffset = fromIntegral unusedRegOffset

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.Posix.Sequence died: "++ show err)
                     Right v -> return v

instance RegexMaker Regex CompOption ExecOption (Seq Char) where
  makeRegexOpts c e pattern = unsafePerformIO $
    (compile c e pattern >>= unwrap)
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $ 
    (compile c e pattern)

instance RegexLike Regex (Seq Char) where
  matchTest regex str = unsafePerformIO $ do
    withSeq str (wrapTest regex) >>= unwrap
  matchOnce regex str = unsafePerformIO $ 
    execute regex str >>= unwrap
  matchAll regex str = unsafePerformIO $ 
    withSeq str (wrapMatchAll regex) >>= unwrap
  matchCount regex str = unsafePerformIO $
    withSeq str (wrapCount regex) >>= unwrap

-- compile
compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> (Seq Char)     -- ^ The regular expression to compile (ASCII only, no null bytes)
         -> IO (Either WrapError Regex) -- ^ Returns: the compiled regular expression
compile flags e pattern =  withSeq pattern (wrapCompile flags e)

-- -----------------------------------------------------------------------------
-- regexec

-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> (Seq Char)     -- ^ (Seq Char) to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --
                -- @
                --   'Just' (array of offset length pairs)
                -- @
execute regex str = do
  maybeStartEnd <- withSeq str (wrapMatch regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
--  Right (Just []) ->  fail "got [] back!" -- return wierd array instead
    Right (Just parts) ->
      return . Right . Just . listArray (0,pred (length parts)) 
       . map (\(s,e)->(fromIntegral s, fromIntegral (e-s)))
       $ parts
    Left err -> return (Left err)

-- -----------------------------------------------------------------------------
-- regexec

-- | Matches a regular expression against a string
regexec :: Regex      -- ^ Compiled regular expression
        -> (Seq Char)     -- ^ (Seq Char) to match against
        -> IO (Either WrapError (Maybe ((Seq Char), (Seq Char), (Seq Char), [(Seq Char)])))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --
                -- @
                --   'Just' (everything before match,
                --         matched portion,
                --         everything after match,
                --         subexpression matches)
                -- @
regexec regex str = do
  let getSub :: (RegOffset,RegOffset) -> (Seq Char)
      getSub (start,stop) | start == unusedRegOffset = S.empty
                          | otherwise = 
        extract (fromEnum start,fromEnum $ stop-start) $ str
      matchedParts :: [(RegOffset,RegOffset)] -> ((Seq Char), (Seq Char), (Seq Char), [(Seq Char)])
      matchedParts [] = (str,S.empty,S.empty,[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) = 
        (before (fromEnum start) str
        ,getSub matchedStartStop
        ,after (fromEnum stop) str
        ,map getSub subStartStop)
  maybeStartEnd <- withSeq str (wrapMatch regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)

withSeq :: Seq Char -> (CString -> IO a) -> IO a
withSeq s f =
  let -- Ensure null at end of s
      s' = case viewr s of                 -- bang !s
             EmptyR -> singleton '\0'
             _ :> '\0' -> s
             _ -> s |> '\0'
      pokes p a = case viewl a of         -- bang pokes !p !a
                    EmptyL -> return ()
                    c :< a' -> poke p (castCharToCChar c) >> pokes (advancePtr p 1) a'
  in allocaBytes (S.length s') (\ptr -> pokes ptr s' >> f ptr)
