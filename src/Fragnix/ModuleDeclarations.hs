{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Symbol (
    Symbol(ValueSymbol,TypeSymbol,InstanceSymbol))
import Fragnix.Primitive (
    loadPrimitiveSymbols)

import Language.Haskell.Exts.Annotated (
    Module,ModuleName(ModuleName),Decl(..),parseFile,ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,QName(Qual),ann,
    prettyPrint,Language(Haskell2010),Extension,
    InstRule(..),InstHead(..),Pat(PVar),Match(Match,InfixMatch))
import Language.Haskell.Names (
    Symbols(Symbols),Error,Scoped(Scoped),computeInterfaces,annotateModule,
    NameInfo(GlobalValue,GlobalType),ModuleNameS)
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,opName,getModuleExtensions)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModuleInfo,modToString)

import Data.Generics.Uniplate.Data (universeBi)

import Data.Set (Set)
import qualified Data.Set as Set (empty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup,insert,elems,fromList)
import Control.Monad.Trans.State.Strict (State,runState,gets,modify)
import qualified Data.Set as Set (fromList)
import Control.Monad (forM)
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe,fromMaybe,maybeToList,listToMaybe)
import Data.Text (pack)

modulDeclarations :: [FilePath] -> IO [Declaration]
modulDeclarations modulpaths = do
    primitivesymbols <- loadPrimitiveSymbols
    modulinformation <- modulDeclarationsNamesExtensions primitivesymbols modulpaths
    return (do
        (declarations,_,ghcextensions) <- Map.elems modulinformation
        Declaration genre _ ast boundsymbols mentionedsymbols <- declarations
        return (Declaration genre ghcextensions ast boundsymbols mentionedsymbols))

modulDeclarationsNamesExtensions :: Map ModuleNameS Symbols -> [FilePath] -> IO ModuleInformation
modulDeclarationsNamesExtensions names modulpaths = do
    asts <- forM modulpaths parse
    let (annotatedasts,newnames) = flip runState names (do
            resolve asts
            forM asts annotate)
    return (Map.fromList (do
        annotatedast <- annotatedasts
        let ModuleName _ modulname = getModuleName annotatedast
            declarations = extractDeclarations annotatedast
            symbols = fromMaybe (Symbols Set.empty Set.empty) (Map.lookup modulname newnames)
            (_,modulextensions) = getModuleExtensions annotatedast
        return (modulname,(declarations,symbols,modulextensions))))

type ModuleInformation = Map ModuleNameS ([Declaration],Symbols,[Extension])

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
    mapMaybe (declToDeclaration modulnameast) (getModuleDecls annotatedast) where
        modulnameast = getModuleName annotatedast

declToDeclaration :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Maybe Declaration
declToDeclaration modulnameast annotatedast = do
    let genre = declGenre annotatedast
    case genre of
        Other -> Nothing
        _ -> return (Declaration
            genre
            []
            (pack (prettyPrint annotatedast))
            (declaredSymbols modulnameast annotatedast)
            (mentionedSymbols annotatedast))

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

mentionedSymbols :: Decl (Scoped SrcSpan) -> [(Maybe ModuleNameS,Symbol)]
mentionedSymbols (TypeSig _ names typ) =
    mapMaybe externalSymbol (universeBi typ) ++
    mapMaybe (fmap noQualification . scopeSymbol . ann) names
mentionedSymbols (InfixDecl _ _ _ ops) =
    mapMaybe (fmap noQualification . scopeSymbol . ann . opName) ops
mentionedSymbols inst@(InstDecl _ _ instrule _) =
    instanceSymbol instrule ++
    mapMaybe pvarSymbol (universeBi inst) ++
    mapMaybe matchSymbol (universeBi inst) ++
    mapMaybe externalSymbol (universeBi inst)
mentionedSymbols inst@(DerivDecl _ _ instrule) =
    instanceSymbol instrule ++
    mapMaybe externalSymbol (universeBi inst)
mentionedSymbols decl = mapMaybe externalSymbol (universeBi decl)

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

instanceSymbol :: InstRule (Scoped SrcSpan) -> [(Maybe ModuleNameS,Symbol)]
instanceSymbol (IParen _ instrule) = instanceSymbol instrule
instanceSymbol (IRule _ _ _ insthead) = do
    classsymbol <- maybeToList (instHeadClassSymbol insthead)
    typesymbol <- maybeToList (listToMaybe (instHeadTypeSymbols insthead))
    return (noQualification (InstanceSymbol classsymbol typesymbol))

instHeadClassSymbol :: InstHead (Scoped SrcSpan) -> Maybe Symbol
instHeadClassSymbol (IHParen _ insthead) = instHeadClassSymbol insthead
instHeadClassSymbol (IHCon _ qname)      = scopeSymbol (ann qname)
instHeadClassSymbol (IHInfix _ _ qname)  = scopeSymbol (ann qname)
instHeadClassSymbol (IHApp _ insthead _) = instHeadClassSymbol insthead

instHeadTypeSymbols :: InstHead (Scoped SrcSpan) -> [Symbol]
instHeadTypeSymbols (IHParen _ insthead) = instHeadTypeSymbols insthead
instHeadTypeSymbols (IHCon _ _)      = []
instHeadTypeSymbols (IHInfix _ typ _)  = mapMaybe scopeSymbol (universeBi typ)
instHeadTypeSymbols (IHApp _ insthead typ) = instHeadTypeSymbols insthead ++ mapMaybe scopeSymbol (universeBi typ)

pvarSymbol :: Pat (Scoped SrcSpan) -> Maybe (Maybe ModuleNameS,Symbol)
pvarSymbol (PVar _ name) = fmap noQualification (scopeSymbol (ann name))
pvarSymbol _ = Nothing

matchSymbol :: Match (Scoped SrcSpan) -> Maybe (Maybe ModuleNameS,Symbol)
matchSymbol (Match _ name _ _ _) = fmap noQualification (scopeSymbol (ann name))
matchSymbol (InfixMatch _ _ name _ _ _) = fmap noQualification (scopeSymbol (ann name))

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
