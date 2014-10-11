module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))

import Language.Haskell.Exts.Annotated (
    Module,ModuleName,Decl(..),
    SrcSpan,
    prettyPrint)
import Language.Haskell.Names (
    Symbols(Symbols),Error,Scoped(Scoped),SymValueInfo,SymTypeInfo,OrigName,
    NameInfo(GlobalValue,GlobalType))
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)

import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Foldable (foldMap)

type NamesPath = FilePath

modulDeclarations :: NamesPath -> [FilePath] -> IO [Declaration]
modulDeclarations namespath modulpaths = do
    asts          <- forM modulpaths parse
    _        <- resolve namespath asts
    annotatedasts <- forM asts (annotate namespath)
    let declarations = concatMap extractDeclarations annotatedasts
    return declarations

parse :: FilePath -> IO (Module SrcSpan)
parse = undefined

resolve :: NamesPath -> [Module l] -> IO (Set (Error l))
resolve = undefined

annotate :: NamesPath -> Module l -> IO (Module (Scoped l))
annotate = undefined

extractDeclarations :: Module (Scoped SrcSpan) -> [Declaration]
extractDeclarations annotatedast =
    map (declToDeclaration modulnameast) (getModuleDecls annotatedast) where
        modulnameast = getModuleName annotatedast

declToDeclaration :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Declaration
declToDeclaration modulnameast annotatedast = Declaration
    (declGenre annotatedast)
    (prettyPrint annotatedast)
    (declaredSymbols modulnameast annotatedast)
    (usedSymbols annotatedast)

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
declaredSymbols modulnameast annotatedast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulnameast annotatedast)

usedSymbols :: Decl (Scoped SrcSpan) -> Symbols
usedSymbols annotatedast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedast)

externalSymbol :: Scoped SrcSpan -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [Left symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [Right symtypeinfo]
externalSymbol _ = []
{-

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
        let builtinpath = builtinPath (toFilePath (fromString (modToString modulename)) <.> "names")
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
        let modulepath = modulePath (modToString modulename)
        createDirectoryIfMissing True (dropFileName modulepath)
        writeInterface modulepath symbols)
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"


-}
