{-# LANGUAGE OverloadedStrings #-}
module Franix.ModuleDeclarations where

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Control.Monad
import Data.Version
import Data.Proxy
import Data.Tagged
import Data.Maybe
import qualified Data.Foldable as F
import System.FilePath
import Text.Printf
import Data.Aeson (encode,ToJSON(toJSON))
import qualified Data.ByteString.Lazy as ByteString (writeFile)

import Language.Haskell.Exts.Annotated (Decl(..))
import Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Set as Set (fromList)

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.Package (PackageIdentifier(pkgName),PackageName(PackageName))
import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity

import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Data.Aeson (object,(.=))

import Data.Either (partitionEithers)
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
    getDBName = Tagged "module-declarations"

theTool :: Compiler.Simple (StandardDB DeclarationsDB)
theTool =
  Compiler.simple
    "module-declarations"
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

parse :: Language -> [Extension] -> CpphsOptions -> FilePath -> IO (HSE.Module HSE.SrcSpan)
parse language extensions cppoptions filename = do
    parseresult <- parseFileWithCommentsAndCPP cppoptions mode filename
    case parseresult of
        UnAnn.ParseOk (ast,_) -> return (fmap srcInfoSpan ast)
        UnAnn.ParseFailed loc msg -> error ("PARSE FAILED: " ++ show loc ++ " " ++ show msg)
  where
    mode = defaultParseMode
             { UnAnn.parseFilename   = filename
             , baseLanguage          = language
             , extensions            = fixExtensions extensions
             , ignoreLanguagePragmas = False
             , ignoreLinePragmas     = False
             , fixities              = Just []
             }

compile :: Compiler.CompileFn
compile builddirectory maybelanguage extensions cppoptions packagename packagedbs dependencies files = do
    let language = fromMaybe Haskell98 maybelanguage

    let isParsec = pkgName packagename == PackageName "parsec"
        cppoptions' = if isParsec then fixCppOptsForParsec cppoptions else fixCppOpts cppoptions

    moduleasts <- mapM (parse language extensions cppoptions') files

    packages <- readPackagesInfo (Proxy :: Proxy (StandardDB DeclarationsDB)) packagedbs dependencies   

    (interfaces, errors) <- evalModuleT (getInterfaces language extensions moduleasts) packages "names" readInterface

    F.for_ errors $ \e -> printf "Warning: %s" (ppError e)

    forM_ (zip moduleasts interfaces) (\(moduleast, symbols) -> do

        annotatedmoduleast <- evalNamesModuleT (annotateModule language extensions moduleast) packages

        let declarations = extractDeclarations (getModuleName annotatedmoduleast) annotatedmoduleast
            HSE.ModuleName _ modulename = getModuleName moduleast
            interfacefilename = builddirectory </> toFilePath (fromString modulename) <.> "names"
            declarationsfilename = builddirectory </> toFilePath (fromString modulename) <.> "declarations"

        createDirectoryIfMissingVerbose silent True (dropFileName interfacefilename)
        createDirectoryIfMissingVerbose silent True (dropFileName declarationsfilename)

        writeInterface interfacefilename $ qualifySymbols packagename symbols
        ByteString.writeFile declarationsfilename (encode declarations))

extractDeclarations :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Module (Scoped HSE.SrcSpan) -> [Declaration]
extractDeclarations modulenameast annotatedmoduleast = map (declToDeclaration modulenameast) (getModuleDecls annotatedmoduleast)

declToDeclaration :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Decl (Scoped HSE.SrcSpan) -> Declaration
declToDeclaration modulenameast annotatedmoduleast = Declaration
    (declGenre annotatedmoduleast)
    (prettyPrint annotatedmoduleast)
    (declaredSymbols modulenameast annotatedmoduleast)
    (usedSymbols annotatedmoduleast)

declGenre :: HSE.Decl (Scoped HSE.SrcSpan) -> Genre
declGenre (TypeDecl _ _ _) = Type
declGenre (TypeFamDecl _ _ _) = Type
declGenre (DataDecl _ _ _ _ _ _) = Type
declGenre (GDataDecl _ _ _ _ _ _ _) = Type
declGenre (DataFamDecl _ _ _ _) = Type
declGenre (TypeInsDecl _ _ _) = Type
declGenre (DataInsDecl _ _ _ _ _) = Type
declGenre (GDataInsDecl _ _ _ _ _ _) = Type
declGenre (ClassDecl _ _ _ _ _) = TypeClass
declGenre (InstDecl _ _ _ _) = ClassInstance
declGenre (DerivDecl _ _ _) = ClassInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = Value
declGenre _ = Other

declaredSymbols :: HSE.ModuleName (Scoped HSE.SrcSpan) -> HSE.Decl (Scoped HSE.SrcSpan) -> Symbols
declaredSymbols modulenameast annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulenameast annotatedmoduleast)

usedSymbols :: HSE.Decl (Scoped HSE.SrcSpan) -> Symbols
usedSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedmoduleast)

externalSymbol :: Scoped HSE.SrcSpan -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [Left symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [Right symtypeinfo]
externalSymbol _ = []

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols UsedSymbols deriving (Show,Eq)
data Genre = Value | TypeSignature | Type | TypeClass | ClassInstance | Other deriving (Show,Eq)
type DeclarationAST = String
type DeclaredSymbols = Symbols
type UsedSymbols = Symbols

instance ToJSON Declaration where
  toJSON (Declaration genre declarationast declaredsymbols usedsymbols) = object [
        "declarationgenre" .= show genre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "mentionedsymbols" .= usedsymbols]

