module Main where

import Fragnix.Environment (
  persistEnvironment)

import Language.Haskell.Exts (
    Module,parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
    ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,SrcLoc(SrcLoc),
    Extension(EnableExtension),KnownExtension(..),
    ModuleName(ModuleName), Name(Ident))
import Language.Haskell.Names (
  resolve, Environment, Symbol(Value, symbolModule))

import Data.Map (
  empty, adjust)
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
      patchedBuiltinEnvironment = patchBuiltinEnvironment builtinEnvironment

  persistEnvironment "fragnix/new_builtin_environment" patchedBuiltinEnvironment


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


-- | The environment we get needs two changes:
--    1. We have to add 'realWorld#' to "GHC.Base"
--    2. We have to rewrite the origin of names exported in "Data.List"
--       They come from "Data.OldList" but this module is hidden
patchBuiltinEnvironment :: Environment -> Environment
patchBuiltinEnvironment = rewriteDataList . addRealWorld

addRealWorld :: Environment -> Environment
addRealWorld = adjust (++ [realWorldSymbol]) ghcBaseModuleName where
  ghcBaseModuleName = ModuleName () "GHC.Base"
  realWorldSymbol = Value ghcBaseModuleName (Ident () "realWorld#")

rewriteDataList :: Environment -> Environment
rewriteDataList = adjust (map rewriteSymbolModule) dataListModuleName where
  dataListModuleName = ModuleName () "Data.List"
  dataOldListModuleName = ModuleName () "Data.OldList"
  rewriteSymbolModule symbol =
    if symbolModule symbol == dataOldListModuleName
      then symbol { symbolModule = dataListModuleName}
      else symbol

