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
    main,CompileFn,Is(..))

import Distribution.Package (PackageIdentifier(pkgName),PackageName(PackageName))
import Distribution.Simple.Compiler (PackageDB(SpecificPackageDB))
import Distribution.Simple.Utils (installOrdinaryFiles)
import Distribution.Verbosity (normal)
import Distribution.Text (display)

import Data.Tagged (Tagged(Tagged))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import System.FilePath ((</>),(<.>),dropFileName)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main =
  Compiler.main HaskellModules

data HaskellModules = HaskellModules

instance Compiler.Is HaskellModules where
    type DB HaskellModules = SpecificDB
    name HaskellModules = "haskell-modules"
    version HaskellModules = Version [0,1] []
    fileExtensions HaskellModules = []
    compile HaskellModules = compile
    languages HaskellModules = knownLanguages
    languageExtensions HaskellModules = knownExtensions
    installLib HaskellModules builddirectory targetdirectory _ _ modulenames = do
        let modulefiles = do
                modulename <- modulenames
                return (builddirectory,display modulename <.> "hs")
        installOrdinaryFiles normal targetdirectory modulefiles

data SpecificDB = SpecificDB FilePath

instance IsPackageDB SpecificDB where
    dbName = Tagged "haskell-modules"
    readPackageDB _ (SpecificDB packagedbfilepath) = do
        packages <- readDB InitDB packagedbfilepath
        return (builtinpackages ++ packages)
    writePackageDB (SpecificDB packagedbfilepath) packages = do
        writeDB packagedbfilepath packages
    locateDB (SpecificPackageDB packagedbfilepath) = return (Just (SpecificDB packagedbfilepath))
    locateDB _ = return Nothing
    globalDB = error "haskell-modules does not support global db"
    dbFromPath = error "haskell-modules does not support db from path"
    userDB = error "haskell-modules does not support user db"

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
    opts {
        defines =
            ("__GLASGOW_HASKELL__", "708") :
            ("INTEGER_SIMPLE", "1") :
            defines opts,
        preInclude = "cabal_macros.h" : preInclude opts,
        includes =
            "/usr/lib/ghc/include/" :
            "~/.haskell-packages/base-4.7.0.0/include/" :
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
        defines = ("__GLASGOW_HASKELL__", "708") : ("INTEGER_SIMPLE", "1") : defines opts,
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
compile builddirectory maybelanguage exts cppoptions packagename _ _ filenames = do

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
            roleStrippedFile = stripRoles preprocessedfile
            parseresult = parseFileContentsWithMode parsemode roleStrippedFile
        case parseresult of
            ParseOk ast -> do
                let modulefilepath = builddirectory </> moduleName ast <.> "hs"
                createDirectoryIfMissing True (dropFileName modulefilepath)
                writeFile modulefilepath roleStrippedFile
            ParseFailed location message -> putStrLn (unlines [
                "PARSE FAILED: " ++ show location ++ " " ++ show message,
                roleStrippedFile]))

moduleName :: Module SrcSpanInfo -> String
moduleName (Module _ (Just (ModuleHead _ (ModuleName _ modulename) _ _)) _ _ _) = modulename
moduleName m = error (show m)

stripRoles :: String -> String
stripRoles = unlines . filter (not . isPrefixOf "type role") . lines
