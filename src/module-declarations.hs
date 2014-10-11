{-# LANGUAGE OverloadedStrings,GeneralizedNewtypeDeriving,TypeFamilies #-}
module Main where

import Language.Haskell.Exts.Annotated (
    Module,ModuleName(ModuleName),Decl,
    defaultParseMode, ParseMode(..),ParseResult(ParseOk,ParseFailed))
import Language.Haskell.Exts.Extension (
    Extension(EnableExtension),KnownExtension(..),knownExtensions,
    Language(..),knownLanguages)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)
import Data.Version
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
import Distribution.Simple.Utils
import Distribution.Verbosity

import Data.Aeson (object,(.=))

import System.Directory (doesFileExist)

import Data.Either (partitionEithers)
import Control.Monad
import Data.Foldable (foldMap)

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
        []

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

parse :: Language -> [Extension] -> CpphsOptions -> FilePath -> IO (Module SrcSpan)
parse language exts cppoptions filename = do
    let mode = defaultParseMode {
            parseFilename   = filename,
            baseLanguage          = language,
            extensions            = fixExtensions exts,
            ignoreLanguagePragmas = False,
            ignoreLinePragmas     = False,
            fixities              = Just []}
    parseresult <- parseFileWithCommentsAndCPP cppoptions mode filename
    case parseresult of
        ParseOk (ast,_) -> return (fmap srcInfoSpan ast)
        ParseFailed location msg -> error ("PARSE FAILED: " ++ show location ++ " " ++ show msg)

compile :: Compiler.CompileFn
compile _ maybelanguage exts cppoptions packagename _ _ files = do
    let language = fromMaybe Haskell98 maybelanguage

    let isParsec = pkgName packagename == PackageName "parsec"
        cppoptions' = if isParsec then fixCppOptsForParsec cppoptions else fixCppOpts cppoptions

    moduleasts <- mapM (parse language exts cppoptions') files  

    (interfaces, errors) <- runFragnixModule (getInterfaces language exts moduleasts)

    F.for_ errors $ \e -> printf "Warning: %s" (ppError e)

    forM_ (zip moduleasts interfaces) (\(moduleast, symbols) -> do

        annotatedmoduleast <- runFragnixModule (annotateModule language exts moduleast)

        let declarations = extractDeclarations (getModuleName annotatedmoduleast) annotatedmoduleast
            ModuleName _ modulename = getModuleName moduleast
            interfacefilename = modulePath modulename
            declarationsfilename = "/home/pschuster/Projects/fragnix/fragnix" </> "declarations" </> modulename

        createDirectoryIfMissingVerbose silent True (dropFileName interfacefilename)
        createDirectoryIfMissingVerbose silent True (dropFileName declarationsfilename)

        writeInterface interfacefilename $ qualifySymbols packagename symbols
        ByteString.writeFile declarationsfilename (encode declarations))

extractDeclarations :: ModuleName (Scoped SrcSpan) -> Module (Scoped SrcSpan) -> [Declaration]
extractDeclarations modulenameast annotatedmoduleast =
  map (declToDeclaration modulenameast) (getModuleDecls annotatedmoduleast)

declToDeclaration :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Declaration
declToDeclaration modulenameast annotatedmoduleast = Declaration
    (declGenre annotatedmoduleast)
    (prettyPrint annotatedmoduleast)
    (declaredSymbols modulenameast annotatedmoduleast)
    (usedSymbols annotatedmoduleast)

declGenre :: Decl (Scoped SrcSpan) -> Genre
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

declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Symbols
declaredSymbols modulenameast annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulenameast annotatedmoduleast)

usedSymbols :: Decl (Scoped SrcSpan) -> Symbols
usedSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedmoduleast)

externalSymbol :: Scoped SrcSpan -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
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

type ModuleNameString = String

modulePath :: ModuleNameString -> FilePath
modulePath modulename = "/home/pschuster/Projects/fragnix/fragnix" </> "names" </> modulename

builtinPath :: ModuleNameString -> FilePath
builtinPath modulename = "/home/pschuster/Projects/fragnix/fragnix" </> "builtin" </> modulename

newtype FragnixModule a = FragnixModule {runFragnixModule :: IO a}
    deriving (Functor,Monad)

instance MonadModule FragnixModule where
    type ModuleInfo FragnixModule = Symbols
    lookupInCache modulename = FragnixModule (do
        let builtinpath = builtinPath (modToString modulename)
            modulepath = modulePath (modToString modulename)
        builtinExists <- doesFileExist builtinpath
        moduleExists <- doesFileExist modulepath
        if builtinExists
            then readInterface builtinpath >>= return . Just
            else (do
                if moduleExists
                    then readInterface modulepath >>= return . Just
                    else return Nothing))
    insertInCache modulename symbols = FragnixModule (do
        writeInterface (modulePath (modToString modulename)) symbols)
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"