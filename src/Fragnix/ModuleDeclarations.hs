{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Symbol (
    Symbol(ValueSymbol,TypeSymbol))
import Fragnix.Primitive (
    primitiveModules)

import Language.Haskell.Exts.Annotated (
    Module,ModuleName(ModuleName),Decl(..),parseFile,ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,QName(Qual),ann,
    prettyPrint,Language(Haskell2010),Extension)
import Language.Haskell.Names (
    Symbols(Symbols),Error,Scoped(Scoped),computeInterfaces,annotateModule,
    NameInfo(GlobalValue,GlobalType),ModuleNameS)
import Language.Haskell.Names.Interfaces (readInterface,writeInterface)
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModuleInfo,modToString)

import Data.Generics.Uniplate.Data (universeBi)

import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import System.FilePath ((</>),dropFileName)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import Data.Text (pack)

type NamesPath = FilePath

modulDeclarations :: [FilePath] -> IO [Declaration]
modulDeclarations modulpaths = do
    asts <- forM modulpaths parse
    _ <- resolve asts
    annotatedasts <- forM asts annotate
    let declarations = concatMap extractDeclarations annotatedasts
    return declarations

parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    parseresult <- parseFile path
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed location message -> error ("PARSE FAILED: " ++ path ++ show location ++ message)

resolve :: [Module SrcSpan] -> IO (Set (Error SrcSpan))
resolve asts = runFragnixModule (computeInterfaces language extensions asts)

annotate :: Module SrcSpan -> IO (Module (Scoped SrcSpan))
annotate ast = runFragnixModule (annotateModule language extensions ast)

language :: Language
language = Haskell2010

extensions :: [Extension]
extensions = []

extractDeclarations :: Module (Scoped SrcSpan) -> [Declaration]
extractDeclarations annotatedast =
    map (declToDeclaration modulnameast) (getModuleDecls annotatedast) where
        modulnameast = getModuleName annotatedast

declToDeclaration :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Declaration
declToDeclaration modulnameast annotatedast = Declaration
    (declGenre annotatedast)
    (pack (prettyPrint annotatedast))
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
declGenre (PatBind _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = Value
declGenre _ = Other

declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Symbols
declaredSymbols modulnameast annotatedast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulnameast annotatedast)

usedSymbols :: Decl (Scoped SrcSpan) -> [(Maybe ModuleNameS,Symbol)]
usedSymbols = mapMaybe externalSymbol . universeBi

externalSymbol :: QName (Scoped SrcSpan) -> Maybe (Maybe ModuleNameS,Symbol)
externalSymbol qname = do
    symbol <- scopeSymbol (ann qname)
    return (case qname of
        Qual _ (ModuleName _ modulname) _ -> (Just modulname,symbol)
        _ -> (Nothing,symbol))

scopeSymbol :: Scoped SrcSpan -> Maybe Symbol
scopeSymbol (Scoped (GlobalValue valuesymbol) _) = Just (ValueSymbol valuesymbol)
scopeSymbol (Scoped (GlobalType typesymbol) _) = Just (TypeSymbol typesymbol)
scopeSymbol _ = Nothing

namesPath :: ModuleNameS -> FilePath
namesPath modulname = "fragnix" </> "names" </> modulname

primitivePath :: ModuleNameS -> FilePath
primitivePath modulname = "fragnix" </> "primitive" </> modulname

newtype FragnixModule a = FragnixModule {runFragnixModule :: IO a}
    deriving (Functor,Monad)

instance MonadModule FragnixModule where
    type ModuleInfo FragnixModule = Symbols
    lookupInCache name = FragnixModule (do
        let modulname = modToString name
        if modulname `elem` primitiveModules
            then do
                fmap Just (readInterface (primitivePath modulname))
            else do
                let namespath = namesPath modulname
                namesExists <- doesFileExist namespath
                if namesExists
                    then fmap Just (readInterface namespath)
                    else return Nothing)
    insertInCache name symbols = FragnixModule (do
        let namespath = namesPath (modToString name)
        createDirectoryIfMissing True (dropFileName namespath)
        writeInterface namespath symbols)
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"
