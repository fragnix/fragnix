module CreateEnv
  ( createEnv
  ) where

import Fragnix.Environment (
  persistEnvironment)
import Fragnix.Paths (
  builtinEnvironmentPath,cbitsPath,includePath)

import Language.Haskell.Exts (
  Module,parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
  ParseResult(ParseOk,ParseFailed),
  SrcSpan,srcInfoSpan,SrcLoc(SrcLoc),
  Extension(EnableExtension),KnownExtension(..),
  ModuleName(ModuleName), Name(Ident))
import Language.Haskell.Names (
  resolve, Environment, Symbol(Value, symbolModule, Data, Constructor))

import qualified Data.Map as Map (
  empty, adjust)
import Data.List (isSuffixOf)
import System.Directory (
  listDirectory, doesFileExist, copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)
import Control.Monad (
  forM, filterM)

createEnv :: IO ()
createEnv = do
  createCbits
  createInclude
  createPackageEnv

-- | Return a list of all files of a given directory
getDirFiles :: FilePath -> IO [FilePath]
getDirFiles fp = do
  filesAndDirs <- map (fp </>) <$> listDirectory fp
  filterM doesFileExist filesAndDirs

-- Include the correct cbits in the fragnix folder

-- | Return a list of all the files ending in "*.c" in builtins/cbits/
getCFiles :: IO [FilePath]
getCFiles = do
  files <- getDirFiles ("builtins" </> "cbits")
  return $ takeFileName <$> (filter (isSuffixOf ".c") files)

createCbits :: IO ()
createCbits = do
  putStrLn "Initializing .fragnix/cbits ..."
  cfiles <- getCFiles
  createDirectoryIfMissing True cbitsPath
  forM cfiles $ \file -> do
    putStrLn $ "   Copying " ++ file ++ "..."
    copyFile ("builtins" </> "cbits" </> file) (cbitsPath </> file)
  return ()

-- Include the correct includes in the fragnix folder

-- | Return a list of all the files ending in "*.h" in builtins/include/
getHFiles :: IO [FilePath]
getHFiles = do
  files <- getDirFiles ("builtins" </> "include")
  return $ takeFileName <$> (filter (isSuffixOf ".h") files)

createInclude :: IO ()
createInclude = do
  putStrLn "Initializing .fragnix/include ..."
  hfiles <- getHFiles
  createDirectoryIfMissing True includePath
  forM hfiles $ \file -> do
    putStrLn $ "   Copying " ++ file ++ "..."
    copyFile ("builtins" </> "include" </> file) (includePath </> file)
  return ()

-- Include the correct Builtin environment
createPackageEnv :: IO ()
createPackageEnv = do
  putStrLn "Initializing builtin environment ..."

  baseFiles <- listDirectory "builtins/base/" >>= return . Prelude.map ("builtins/base/" ++)
  ghcPrimFiles <- listDirectory "builtins/ghc-prim/" >>= return . Prelude.map ("builtins/ghc-prim/" ++)
  integerGmpFiles <- listDirectory "builtins/integer-gmp/" >>= return . Prelude.map ("builtins/integer-gmp/" ++)

  baseModules <- forM baseFiles parse
  ghcPrimModules <- forM ghcPrimFiles parse
  integerGmpModules <- forM integerGmpFiles parse

  let builtinEnvironment = resolve (baseModules ++ ghcPrimModules ++ integerGmpModules) Map.empty
      patchedBuiltinEnvironment = patchBuiltinEnvironment builtinEnvironment

  persistEnvironment builtinEnvironmentPath patchedBuiltinEnvironment


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
   rewriteFloat .
   rewriteLazyST .
   rewriteForeignPtr .
   rewriteGHCIntegerType .
   rewriteInteger .
   rewriteBigNat .
   rewriteDataList .
   addRealWorld

addRealWorld :: Environment -> Environment
addRealWorld = Map.adjust (++ [realWorldSymbol]) ghcBaseModuleName where
  ghcBaseModuleName = ModuleName () "GHC.Base"
  realWorldSymbol = Value ghcBaseModuleName (Ident () "realWorld#")

rewriteSymbolModuleInModules :: String -> String -> [String] -> Environment -> Environment
rewriteSymbolModuleInModules fromModuleName toModuleName inModules =
  foldr (.) id (map (Map.adjust (map rewriteSymbol) . ModuleName ()) inModules) where
    rewriteSymbol symbol = if symbolModule symbol == ModuleName () fromModuleName
        then symbol { symbolModule = ModuleName () toModuleName }
        else symbol

rewriteDataList :: Environment -> Environment
rewriteDataList = rewriteSymbolModuleInModules "Data.OldList" "Data.List" ["Data.List","Prelude","Data.String"]

rewriteGHCIntegerType :: Environment -> Environment
rewriteGHCIntegerType = rewriteSymbolModuleInModules "GHC.Integer.Type" "GHC.Integer" ["Prelude", "GHC.Num", "GHC.Integer"]

rewriteForeignPtr :: Environment -> Environment
rewriteForeignPtr = rewriteSymbolModuleInModules "Foreign.ForeignPtr.Imp" "Foreign.ForeignPtr" ["Foreign", "Foreign.ForeignPtr", "Foreign.Safe", "Foreign.ForeignPtr.Safe"]

rewriteLazyST :: Environment -> Environment
rewriteLazyST = rewriteSymbolModuleInModules "Control.Monad.ST.Lazy.Imp" "Control.Monad.ST.Lazy"  ["Control.Monad.ST.Lazy", "Control.Monad.ST.Lazy.Safe", "Control.Monad.ST.Lazy.Unsafe"]

rewriteSymbolInModules :: Symbol -> Symbol -> [String] -> Environment -> Environment
rewriteSymbolInModules fromSymbol toSymbol inModules =
  foldr (.) id (map (Map.adjust (map rewriteSymbol) . ModuleName ()) inModules) where
    rewriteSymbol symbol = if symbol == fromSymbol
        then toSymbol
        else symbol

rewriteFloat :: Environment -> Environment
rewriteFloat = foldr (.) id [
  rewriteSymbolInModules
    (Data (ModuleName () "GHC.Types") (Ident () "Double"))
    (Data (ModuleName () "GHC.Float") (Ident () "Double"))
    ["Prelude", "GHC.Exts"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Types") (Ident () "D#") (Ident () "Double"))
    (Constructor (ModuleName () "GHC.Float") (Ident () "D#") (Ident () "Double"))
    ["Prelude", "GHC.Exts"],
  rewriteSymbolInModules
    (Data (ModuleName () "GHC.Types") (Ident () "Float"))
    (Data (ModuleName () "GHC.Float") (Ident () "Float"))
    ["Prelude", "GHC.Exts"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Types") (Ident () "F#") (Ident () "Float"))
    (Constructor (ModuleName () "GHC.Float") (Ident () "F#") (Ident () "Float"))
    ["Prelude", "GHC.Exts"]]

-- | It is important that this comes before 'rewriteGHCIntegerType'
rewriteBigNat :: Environment -> Environment
rewriteBigNat = foldr (.) id [
  rewriteSymbolInModules
    (Data (ModuleName () "GHC.Integer.Type") (Ident () "BigNat"))
    (Data (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "BigNat"))
    ["GHC.Integer.GMP.Internals"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Integer.Type") (Ident () "BN#") (Ident () "BigNat"))
    (Constructor (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "BN#") (Ident () "BigNat"))
    ["GHC.Natural", "GHC.Integer.GMP.Internals"]]

-- | It is important that this comes before 'rewriteGHCIntegerType'
rewriteInteger :: Environment -> Environment
rewriteInteger = foldr (.) id [
  rewriteSymbolInModules
    (Data (ModuleName () "GHC.Integer.Type") (Ident () "Integer"))
    (Data (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "Integer"))
    ["Prelude", "GHC.Num", "GHC.Integer", "GHC.Integer.GMP.Internals"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Integer.Type") (Ident () "S#") (Ident () "Integer"))
    (Constructor (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "S#") (Ident () "Integer"))
    ["Prelude", "GHC.Num", "GHC.Integer", "GHC.Integer.GMP.Internals"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Integer.Type") (Ident () "Jp#") (Ident () "Integer"))
    (Constructor (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "Jp#") (Ident () "Integer"))
    ["Prelude", "GHC.Num", "GHC.Integer", "GHC.Integer.GMP.Internals"],
  rewriteSymbolInModules
    (Constructor (ModuleName () "GHC.Integer.Type") (Ident () "Jn#") (Ident () "Integer"))
    (Constructor (ModuleName () "GHC.Integer.GMP.Internals") (Ident () "Jn#") (Ident () "Integer"))
    ["Prelude", "GHC.Num", "GHC.Integer", "GHC.Integer.GMP.Internals"]]

