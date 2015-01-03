{-# LINE 1 "./Text/Regex/Posix/String.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                           






                          






                                 






                                






                               






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Text/Regex/Posix/String.hs" #-}
{-# LINE 1 "./Text/Regex/Posix/String.hs" #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Posix.String
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

module Text.Regex.Posix.String(
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
  -- ** Execution options
  ExecOption(ExecOption),
  execBlank,
  execNotBOL,     -- not at begining of line
  execNotEOL     -- not at end of line
  ) where

import Data.Array(listArray, Array)
import Data.List(genericDrop, genericTake)
import Foreign.C.String(withCAString)
import System.IO.Unsafe(unsafePerformIO)
import Text.Regex.Base.RegexLike(RegexContext(..),RegexMaker(..),RegexLike(..),MatchOffset,MatchLength)
import Text.Regex.Posix.Wrap
import Text.Regex.Base.Impl(polymatch,polymatchM)

instance RegexContext Regex String String where
  match = polymatch
  matchM = polymatchM

unusedOffset :: Int
unusedOffset = fromIntegral unusedRegOffset

unwrap :: (Show e) => Either e v -> IO v
unwrap x = case x of Left err -> fail ("Text.Regex.Posix.String died: "++ show err)
                     Right v -> return v

instance RegexMaker Regex CompOption ExecOption String where
  makeRegexOpts c e pattern = unsafePerformIO $
    (compile c e pattern >>= unwrap)
  makeRegexOptsM c e pattern = either (fail.show) return $ unsafePerformIO $ 
    (compile c e pattern)

instance RegexLike Regex String where
  matchTest regex str = unsafePerformIO $ do
    withCAString str (wrapTest regex) >>= unwrap
  matchOnce regex str = unsafePerformIO $ 
    execute regex str >>= unwrap
  matchAll regex str = unsafePerformIO $ 
    withCAString str (wrapMatchAll regex) >>= unwrap
  matchCount regex str = unsafePerformIO $
    withCAString str (wrapCount regex) >>= unwrap

-- compile
compile  :: CompOption -- ^ Flags (summed together)
         -> ExecOption -- ^ Flags (summed together)
         -> String     -- ^ The regular expression to compile (ASCII only, no null bytes)
         -> IO (Either WrapError Regex) -- ^ Returns: the compiled regular expression
compile flags e pattern =  withCAString pattern (wrapCompile flags e)

-- -----------------------------------------------------------------------------
-- regexec

-- | Matches a regular expression against a string
execute :: Regex      -- ^ Compiled regular expression
        -> String     -- ^ String to match against
        -> IO (Either WrapError (Maybe (Array Int (MatchOffset,MatchLength))))
                -- ^ Returns: 'Nothing' if the regex did not match the
                -- string, or:
                --
                -- @
                --   'Just' (array of offset length pairs)
                -- @
execute regex str = do
  maybeStartEnd <- withCAString str (wrapMatch regex)
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
        -> String     -- ^ String to match against
        -> IO (Either WrapError (Maybe (String, String, String, [String])))
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
  let getSub (start,stop) | start == unusedRegOffset = ""
                          | otherwise = 
        genericTake (stop-start) . genericDrop start $ str
      matchedParts [] = (str,"","",[]) -- no information
      matchedParts (matchedStartStop@(start,stop):subStartStop) = 
        (genericTake start str
        ,getSub matchedStartStop
        ,genericDrop stop str
        ,map getSub subStartStop)
  maybeStartEnd <- withCAString str (wrapMatch regex)
  case maybeStartEnd of
    Right Nothing -> return (Right Nothing)
    Right (Just parts) -> return . Right . Just . matchedParts $ parts
    Left err -> return (Left err)
