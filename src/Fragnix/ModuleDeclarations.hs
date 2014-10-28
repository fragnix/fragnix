{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Symbol (
    Symbol(ValueSymbol,TypeSymbol))
import Fragnix.Primitive (
    loadPrimitiveSymbols)

import Language.Haskell.Exts.Annotated (
    Module,ModuleName(ModuleName),Decl(..),parseFile,ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,QName(Qual),ann,
    prettyPrint,Language(Haskell2010),Extension)
import Language.Haskell.Names (
    Symbols(Symbols),Error,Scoped(Scoped),computeInterfaces,annotateModule,
    NameInfo(GlobalValue,GlobalType),ModuleNameS)
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,opName)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModuleInfo,modToString)

import Data.Generics.Uniplate.Data (universeBi)

import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup,insert)
import Control.Monad.Trans.State.Strict (State,runState,gets,modify)
import qualified Data.Set as Set (fromList)
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import Data.Text (pack)

type NamesPath = FilePath

modulDeclarations :: [FilePath] -> IO [Declaration]
modulDeclarations modulpaths = do
    primitivesymbols <- loadPrimitiveSymbols
    fmap fst (modulDeclarationsAndNames primitivesymbols modulpaths)

modulDeclarationsAndNames :: Map ModuleNameS Symbols -> [FilePath] -> IO ([Declaration],Map ModuleNameS Symbols)
modulDeclarationsAndNames names modulpaths = do
    asts <- forM modulpaths parse
    let (annotatedasts,newnames) = flip runState names (do
            resolve asts
            forM asts annotate)
    let declarations = concatMap extractDeclarations annotatedasts
    return (declarations,newnames)

parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    parseresult <- parseFile path
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed location message -> error ("PARSE FAILED: " ++ path ++ show location ++ message)

resolve :: [Module SrcSpan] -> State (Map ModuleNameS Symbols) (Set (Error SrcSpan))
resolve asts = runFragnixModule (computeInterfaces language extensions asts)

annotate :: Module SrcSpan -> State (Map ModuleNameS Symbols) (Module (Scoped SrcSpan))
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
declGenre (InfixDecl _ _ _ _) = InfixFixity
declGenre _ = Other

declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Symbols
declaredSymbols modulnameast annotatedast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulnameast annotatedast)

usedSymbols :: Decl (Scoped SrcSpan) -> [(Maybe ModuleNameS,Symbol)]
usedSymbols (TypeSig _ names typ) =
    mapMaybe externalSymbol (universeBi typ) ++
    mapMaybe (fmap noQualification . scopeSymbol . ann) names
usedSymbols (InfixDecl _ _ _ ops) =
    mapMaybe (fmap noQualification . scopeSymbol . ann . opName) ops
usedSymbols decl = mapMaybe externalSymbol (universeBi decl)

externalSymbol :: QName (Scoped SrcSpan) -> Maybe (Maybe ModuleNameS,Symbol)
externalSymbol qname = do
    symbol <- scopeSymbol (ann qname)
    return (case qname of
        Qual _ (ModuleName _ modulname) _ -> (Just modulname,symbol)
        _ -> noQualification symbol)

scopeSymbol :: Scoped SrcSpan -> Maybe Symbol
scopeSymbol (Scoped (GlobalValue valuesymbol) _) = Just (ValueSymbol valuesymbol)
scopeSymbol (Scoped (GlobalType typesymbol) _) = Just (TypeSymbol typesymbol)
scopeSymbol _ = Nothing

noQualification :: Symbol -> (Maybe ModuleNameS,Symbol)
noQualification symbol = (Nothing,symbol)

newtype FragnixModule a = FragnixModule {runFragnixModule :: State (Map ModuleNameS Symbols) a}
    deriving (Functor,Monad)

instance MonadModule FragnixModule where
    type ModuleInfo FragnixModule = Symbols
    lookupInCache name = FragnixModule (do
        let modulname = modToString name
        gets (Map.lookup modulname))
    insertInCache name symbols = FragnixModule (do
        modify (Map.insert (modToString name) symbols))
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"
