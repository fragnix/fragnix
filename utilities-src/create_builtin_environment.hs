module Main where

import Fragnix.Environment (
  persistEnvironment)

import Language.Haskell.Exts (
    Module,parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
    ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,SrcLoc(SrcLoc),
    Extension(EnableExtension),KnownExtension(..))
import Language.Haskell.Names (
  resolve)

import Data.Map (
  empty)
import System.Directory (
  listDirectory)
import Control.Monad (
  forM)


main :: IO ()
main = do

  baseFiles <- listDirectory "tests/packages/base/" >>= return . Prelude.map ("tests/packages/base/" ++)
  ghcPrimFiles <- listDirectory "tests/packages/ghc-prim/" >>= return . Prelude.map ("tests/packages/ghc-prim/" ++)
  integerGmpFiles <- listDirectory "tests/packages/integer-gmp/" >>= return . Prelude.map ("tests/packages/integer-gmp/" ++)

  baseModules <- forM baseFiles parse
  ghcPrimModules <- forM ghcPrimFiles parse
  integerGmpModules <- forM integerGmpFiles parse

  let builtinEnvironment = resolve (baseModules ++ ghcPrimModules ++ integerGmpModules) Data.Map.empty

  persistEnvironment "fragnix/new_builtin_environment" builtinEnvironment


parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    fileContents <- readFile path
    let parseMode = defaultParseMode {
            parseFilename = path,
            extensions = globalExtensions,
            fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode parseMode fileContents
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed (SrcLoc filename line column) message -> error (unlines [
            "failed to parse module.",
            "filename: " ++ filename,
            "line: " ++ show line,
            "column: " ++ show column,
            "error: " ++ message])


globalExtensions :: [Extension]
globalExtensions = [
    EnableExtension MultiParamTypeClasses,
    EnableExtension NondecreasingIndentation,
    EnableExtension ExplicitForAll,
    EnableExtension PatternGuards,
    EnableExtension ExplicitNamespaces,
    EnableExtension FlexibleContexts,
    EnableExtension DataKinds,
    EnableExtension KindSignatures,
    EnableExtension PolyKinds]


