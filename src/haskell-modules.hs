{-# LANGUAGE OverloadedStrings,GeneralizedNewtypeDeriving,TypeFamilies #-}
module Main where

import Fragnix.BuiltinPackages (builtinpackages)

import Language.Haskell.Exts.Annotated (
    Module(Module),ModuleHead(ModuleHead),ModuleName(ModuleName),
    parseFileContentsWithMode,
    ParseMode(..),defaultParseMode,baseLanguage,extensions,
    ParseResult(ParseOk,ParseFailed),SrcSpanInfo)
import Language.Haskell.Exts.Extension (
    Extension(EnableExtension),KnownExtension(..),knownExtensions,
    Language(..),knownLanguages)
import Language.Preprocessor.Cpphs (
    runCpphs,CpphsOptions(..),BoolOptions(..))
import Data.Version (Version(Version))

import Distribution.HaskellSuite (
    IsPackageDB(..),readDB,writeDB,MaybeInitDB(InitDB))
import qualified Distribution.HaskellSuite.Compiler as Compiler (
    main,Simple,simple,CompileFn)

import Distribution.Package (
    PackageIdentifier(pkgName),PackageName(PackageName))

import Data.Tagged (Tagged(Tagged))
import System.Directory (createDirectoryIfMissing)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.FilePath ((</>),(<.>),dropFileName)

main :: IO ()
main =
  Compiler.main theTool

theTool :: Compiler.Simple DummyDB
theTool =
    Compiler.simple
        "haskell-modules"
        version
        knownLanguages
        knownExtensions
        compile
        []

version :: Version
version = Version [0,1] []

data DummyDB = DummyDB

instance IsPackageDB DummyDB where
    dbName = Tagged "haskell-modules"
    readPackageDB _ DummyDB = do
        packages <- readDB InitDB packagedbfilename
        return (builtinpackages ++ packages)
    writePackageDB DummyDB packages = writeDB packagedbfilename packages
    globalDB = return (Just DummyDB)
    dbFromPath _ = return DummyDB
    locateDB _ = return (Just DummyDB)
    userDB = return DummyDB

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
    opts {
        defines =
            ("__GLASGOW_HASKELL__", "706") :
            ("INTEGER_SIMPLE", "1") :
            defines opts,
        preInclude = "cabal_macros.h" : preInclude opts,
        includes =
            "/usr/lib/ghc/include/" :
            "/home/pschuster/.haskell-packages/base-4.7.0.0/include/" :
            includes opts,
        boolopts = fixBoolOpts (boolopts opts)
    }

fixBoolOpts :: BoolOptions -> BoolOptions
fixBoolOpts bo =
    bo {
        lang = False
  }

fixCppOptsForParsec :: CpphsOptions -> CpphsOptions
fixCppOptsForParsec opts =
    opts {
        defines = ("__GLASGOW_HASKELL__", "706") : ("INTEGER_SIMPLE", "1") : defines opts,
        includes = "/usr/lib/ghc/include/": includes opts,
        boolopts = fixBoolOptsForParsec (boolopts opts)
    }

fixBoolOptsForParsec :: BoolOptions -> BoolOptions
fixBoolOptsForParsec bo =
    bo {
        lang = False,
        stripC89 = False
  }

fixExtensions :: [Extension] -> [Extension]
fixExtensions exts =
    (EnableExtension MultiParamTypeClasses):
    (EnableExtension NondecreasingIndentation):
    (EnableExtension TypeOperators):
    (EnableExtension BangPatterns):
    (EnableExtension TemplateHaskell):
    exts

compile :: Compiler.CompileFn
compile _ maybelanguage exts cppoptions packagename _ _ filenames = do

    let language = fromMaybe Haskell98 maybelanguage
        isParsec = pkgName packagename == PackageName "parsec"
        cppoptions' = if isParsec then fixCppOptsForParsec cppoptions else fixCppOpts cppoptions

    forM_ filenames (\filename -> do
        file <- readFile filename
        preprocessedfile <- runCpphs cppoptions' filename file
        let parsemode = defaultParseMode {
                parseFilename         = filename,
                baseLanguage          = language,
                extensions            = fixExtensions exts,
                ignoreLanguagePragmas = False,
                ignoreLinePragmas     = False,
                fixities              = Just []}
            parseresult = parseFileContentsWithMode parsemode preprocessedfile
        case parseresult of
            ParseOk ast -> do
                createDirectoryIfMissing True (dropFileName (modulfilename ast))
                writeFile (modulfilename ast) preprocessedfile
            ParseFailed location message -> error ("PARSE FAILED: " ++ show location ++ " " ++ show message))

modulfilename :: Module SrcSpanInfo -> FilePath
modulfilename (Module _ (Just (ModuleHead _ (ModuleName _ modulname) _ _)) _ _ _) =
    "/home/pschuster/Projects/fragnix/temp/fragnix/modules" </> modulname <.> "hs"

packagedbfilename :: FilePath
packagedbfilename = "/home/pschuster/Projects/fragnix/temp/fragnix/packagedb/packages.db"
