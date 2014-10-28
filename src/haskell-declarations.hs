{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.ModuleDeclarations (parse,extractDeclarations)

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Parser (fromParseResult)
import Language.Preprocessor.Cpphs (runCpphsReturningSymTab)
import Control.Monad
import Control.Exception
import Data.Version
import Data.Typeable
import Data.Proxy
import Data.Tagged
import Data.Maybe
import qualified Data.Foldable as F
import System.FilePath
import System.Directory
import Text.Printf
import Data.Aeson (encode,ToJSON(toJSON))
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import qualified Language.Haskell.Exts.Annotated as HSE (Module,Decl,SrcSpan,ModuleName)
import Language.Haskell.Exts.Annotated (Decl(..))
import Language.Haskell.Exts.Extension (Language(Haskell2010))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set as Set (fromList)

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.Package (PackageIdentifier(pkgName),PackageName(PackageName))
import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Verbosity

import Language.Haskell.Names (
    Symbols(Symbols),annotateModule,Scoped(Scoped),SymValueInfo,SymTypeInfo,OrigName,
    NameInfo(GlobalValue,GlobalType))
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Distribution.ModuleName (ModuleName)
import Distribution.Text (display)

import Data.Aeson (encode,ToJSON(toJSON),object,(.=))

import qualified Data.ByteString.Lazy as ByteString (writeFile)
import Control.Monad (forM_,when)
import Data.Either (partitionEithers)
import qualified Data.Set as Set (fromList)
import Data.Foldable (foldMap)

import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Names.SyntaxUtils

main :: IO ()
main =
  Compiler.main theTool

version :: Data.Version.Version
version = Data.Version.Version [0,1] []

data DeclarationsDB = DeclarationsDB

instance IsDBName DeclarationsDB where
    getDBName = Tagged "haskell-declarations"

theTool :: Compiler.Simple (StandardDB DeclarationsDB)
theTool =
  Compiler.simple
    "haskell-declarations"
    version
    knownLanguages
    knownExtensions
    compile
    ["names","declarations"]

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
fixBoolOpts boolopts =
  boolopts {
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
fixBoolOptsForParsec boolopts =
  boolopts {
    lang = False,
    stripC89 = False
}

fixExtensions :: [Extension] -> [Extension]
fixExtensions extensions =
  (EnableExtension MultiParamTypeClasses):
  (EnableExtension NondecreasingIndentation):
  (EnableExtension TypeOperators):
  (EnableExtension BangPatterns):
  (EnableExtension TemplateHaskell):
  extensions

compile :: Compiler.CompileFn
compile builddirectory maybelanguage extensions cppoptions packagename packagedbs dependencies files = do
    let language = fromMaybe Haskell98 maybelanguage

    let isParsec = pkgName packagename == PackageName "parsec"
        cppoptions' = if isParsec then fixCppOptsForParsec cppoptions else fixCppOpts cppoptions

    modulasts <- mapM parse files

    packages <- readPackagesInfo (Proxy :: Proxy (StandardDB DeclarationsDB)) packagedbs dependencies   

    (interfaces, errors) <- evalModuleT (getInterfaces language extensions modulasts) packages "names" readInterface

    F.for_ errors $ \e -> printf "Warning: %s" (ppError e)

    forM_ (zip modulasts interfaces) (\(modulast, symbols) -> do

        annotatedmodulast <- evalNamesModuleT (annotateModule language extensions modulast) packages

        let declarations = extractDeclarations annotatedmodulast
            HSE.ModuleName _ modulname = getModuleName modulast
            interfacefilename = builddirectory </> toFilePath (fromString modulname) <.> "names"
            declarationsfilename = builddirectory </> toFilePath (fromString modulname) <.> "declarations"

        createDirectoryIfMissingVerbose silent True (dropFileName interfacefilename)
        createDirectoryIfMissingVerbose silent True (dropFileName declarationsfilename)

        writeInterface interfacefilename $ qualifySymbols packagename symbols
        ByteString.writeFile declarationsfilename (encode declarations))
