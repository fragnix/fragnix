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

import qualified Data.Map as Map (
  empty, adjust)
import System.Directory (
  listDirectory)
import Control.Monad (
  forM)


{-

Steps to extract preprocessed module files from base, ghc-prim and integer-gmp
delete non-source files and folders
Do those before invoking this executable

find */ -type f -exec bash -c 'file=${1#./}; mv "$file" "${file//\//.}"' _ '{}' \;

delete now-empty folders

rm *.hs-boot
rm *.hsc
ghc -E * -I../../../../base-4.9.1.0/include -optP -P -optL -P
rm *.hs
rename 's/.hspp$/.hs/' *.hspp

-}

main :: IO ()
main = do

  baseFiles <- listDirectory "tests/packages/base/" >>= return . Prelude.map ("tests/packages/base/" ++)
  ghcPrimFiles <- listDirectory "tests/packages/ghc-prim/" >>= return . Prelude.map ("tests/packages/ghc-prim/" ++)
  integerGmpFiles <- listDirectory "tests/packages/integer-gmp/" >>= return . Prelude.map ("tests/packages/integer-gmp/" ++)

  baseModules <- forM baseFiles parse
  ghcPrimModules <- forM ghcPrimFiles parse
  integerGmpModules <- forM integerGmpFiles parse

  let builtinEnvironment = resolve (baseModules ++ ghcPrimModules ++ integerGmpModules) Map.empty
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
--    2. We have to rewrite the origin of names exported in several modules
--       The reason is that the origin is hidden
patchBuiltinEnvironment :: Environment -> Environment
patchBuiltinEnvironment =
   rewriteLazyST . rewriteForeignPtr . rewriteGHCInteger . rewriteDataList . addRealWorld

addRealWorld :: Environment -> Environment
addRealWorld = Map.adjust (++ [realWorldSymbol]) ghcBaseModuleName where
  ghcBaseModuleName = ModuleName () "GHC.Base"
  realWorldSymbol = Value ghcBaseModuleName (Ident () "realWorld#")

rewriteSymbolModule :: String -> String -> [String] -> Environment -> Environment
rewriteSymbolModule fromModuleName toModuleName inModules =
  foldr (.) id (map (Map.adjust (map rewriteSymbol) . ModuleName ()) inModules) where
    rewriteSymbol symbol = if symbolModule symbol == ModuleName () fromModuleName
        then symbol { symbolModule = ModuleName () toModuleName }
        else symbol

rewriteDataList :: Environment -> Environment
rewriteDataList = rewriteSymbolModule "Data.OldList" "Data.List" ["Data.List"]

rewriteGHCInteger :: Environment -> Environment
rewriteGHCInteger = rewriteSymbolModule "GHC.Integer.Type" "GHC.Integer" ["Prelude", "GHC.Num", "GHC.Integer"]

rewriteForeignPtr :: Environment -> Environment
rewriteForeignPtr = rewriteSymbolModule "Foreign.ForeignPtr.Imp" "Foreign.ForeignPtr" ["Foreign", "Foreign.ForeignPtr", "Foreign.Safe", "Foreign.ForeignPtr.Safe"]

rewriteLazyST :: Environment -> Environment
rewriteLazyST = rewriteSymbolModule "Control.Monad.ST.Lazy.Imp" "Control.Monad.ST.Lazy"  ["Control.Monad.ST.Lazy", "Control.Monad.ST.Lazy.Safe", "Control.Monad.ST.Lazy.Unsafe"]

