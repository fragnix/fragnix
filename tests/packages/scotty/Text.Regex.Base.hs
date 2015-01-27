{-# LINE 1 "Text/Regex/Base.hs" #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
--
-- Module      :  Text.Regex.Base
-- Copyright   :  (c) Chris Kuklewicz 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org, textregexlazy@personal.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTC+FD)
--
-- Classes and instances for Regex matching.
--
-- 
-- This module merely imports and re-exports the common part of the new
-- api: "Text.Regex.Base.RegexLike" and "Text.Regex.Base.Context".
-- 
-- To see what result types the instances of RegexContext can produce,
-- please read the "Text.Regex.Base.Context" haddock documentation.
-- 
-- This does not provide any of the backends, just the common interface
-- they all use.  The modules which provide the backends and their cabal
-- packages are:
-- 
--  * @Text.Regex.Posix@ from regex-posix
-- 
--  * @Text.Regex@ from regex-compat (uses regex-posix)
-- 
--  * @Text.Regex.Parsec@ from regex-parsec
-- 
--  * @Text.Regex.DFA@ from regex-dfa
-- 
--  * @Text.Regex.PCRE@ from regex-pcre
-- 
--  * @Test.Regex.TRE@ from regex-tre
-- 
-- In fact, just importing one of the backends is adequate, you do not
-- also need to import this module.
-- 
-- TODO: Copy Example*hs files into this haddock comment
-----------------------------------------------------------------------------

module Text.Regex.Base (getVersion_Text_Regex_Base
  -- | RegexLike defines classes and type, and 'Extract' instances
  ,module Text.Regex.Base.RegexLike) where

import Data.Version(Version(..))
import Text.Regex.Base.RegexLike
import Text.Regex.Base.Context()

getVersion_Text_Regex_Base :: Version
getVersion_Text_Regex_Base =
  Version { versionBranch = [0,93,2]
          , versionTags = ["unstable"]
          }
