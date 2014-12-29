{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Environment (
    loadSymbols,persistSymbols,primitiveSymbolsPath,symbolsPath)

import qualified Language.Haskell.Exts as UnAnn (
    QName(Qual,UnQual),ModuleName(ModuleName))
import Language.Haskell.Exts.Annotated.Simplify (sModuleName)
import Language.Haskell.Exts.Annotated (
    Module,Decl(..),parseFile,ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,ModuleName,
    prettyPrint,Language(Haskell2010),Extension)
import Language.Haskell.Names (
    Symbol(NewType,Constructor),Error,Scoped(Scoped),
    computeInterfaces,annotateModule,
    NameInfo(GlobalSymbol,RecPatWildcard),ppError)
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,getModuleExtensions)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModuleInfo,modToString)


import Data.Set (Set)
import qualified Data.Set as Set (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (
    lookup,insert,elems,fromList,union,map)
import Control.Monad.Trans.State.Strict (
    State,runState,gets,modify)
import Control.Monad (forM)
import Control.Applicative (Applicative,(<$>),(<*>))
import Data.Maybe (mapMaybe,maybeToList)
import Data.Text (pack)
import Data.Foldable (toList)
import Data.List (nub)

moduleDeclarations :: [FilePath] -> IO [Declaration]
moduleDeclarations modulpaths = do
    primitivesymbols <- loadSymbols primitiveSymbolsPath
    symbols <- loadSymbols symbolsPath
    modulinformation <- moduleDeclarationsNamesExtensions (Map.union primitivesymbols symbols) modulpaths
    persistSymbols symbolsPath (Map.map (\(_,modulsymbols,_) -> modulsymbols) modulinformation)
    return (do
        (declarations,_,ghcextensions) <- Map.elems modulinformation
        Declaration genre _ ast boundsymbols mentionedsymbols <- declarations
        return (Declaration genre ghcextensions ast boundsymbols mentionedsymbols))

moduleDeclarationsNamesExtensions :: Map UnAnn.ModuleName [Symbol] -> [FilePath] -> IO ModuleInformation
moduleDeclarationsNamesExtensions names modulpaths = do
    asts <- forM modulpaths parse
    let ((errors,annotatedasts),newnames) = flip runState names (do
            (,) <$> resolve asts <*> forM asts annotate)
    forM (Set.toList errors) (putStrLn . ("WARNING: " ++) . ppError)
    return (Map.fromList (do
        annotatedast <- annotatedasts
        let modulname = sModuleName (getModuleName annotatedast)
            declarations = extractDeclarations annotatedast
            symbols = concat (maybeToList (Map.lookup modulname newnames))
            (_,modulextensions) = getModuleExtensions annotatedast
        return (modulname,(declarations,symbols,modulextensions))))

type ModuleInformation = Map UnAnn.ModuleName ([Declaration],[Symbol],[Extension])

parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    parseresult <- parseFile path
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed location message -> error ("PARSE FAILED: " ++ path ++ show location ++ message)

resolve :: [Module SrcSpan] -> State (Map UnAnn.ModuleName [Symbol]) (Set (Error SrcSpan))
resolve asts = runFragnixModule (computeInterfaces language extensions asts)

annotate :: Module SrcSpan -> State (Map UnAnn.ModuleName [Symbol]) (Module (Scoped SrcSpan))
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

-- | The genre of a declaration, for example Type, Value, TypeSignature, ...
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

-- | All symbols the given declaration in a module with the given name binds.
declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> [Symbol]
declaredSymbols modulnameast annotatedast = getTopDeclSymbols GlobalTable.empty modulnameast annotatedast

-- | All symbols the given declaration mentions together with a qualifiaction
-- if they are used qualified. Foreign imports have an implicit dependency on
-- the constructors of all mentioned newtypes.
mentionedSymbols :: Decl (Scoped SrcSpan) -> [(Symbol,Maybe UnAnn.ModuleName)]
mentionedSymbols decl@(ForImp _ _ _ _ _ _) = newtypeconstructors ++ mentionedsymbols where
    mentionedsymbols = nub (concatMap scopeSymbol (toList decl))
    newtypeconstructors = do
        (NewType symbolmodule symbolname,_) <- mentionedsymbols
        return (Constructor symbolmodule symbolname symbolname,Nothing)
mentionedSymbols decl = nub (concatMap scopeSymbol (toList decl))

-- | Get all references to global symbols from the given scope annotation.
scopeSymbol :: Scoped SrcSpan -> [(Symbol,Maybe UnAnn.ModuleName)]
scopeSymbol (Scoped (GlobalSymbol symbol (UnAnn.Qual modulname _)) _) = [(symbol,Just modulname)]
scopeSymbol (Scoped (GlobalSymbol symbol (UnAnn.UnQual _)) _) = [(symbol,Nothing)]
scopeSymbol (Scoped (RecPatWildcard symbols) _) = map (\symbol -> (symbol,Nothing)) symbols
scopeSymbol _ = []

newtype FragnixModule a = FragnixModule {runFragnixModule :: State (Map UnAnn.ModuleName [Symbol]) a}
    deriving (Functor,Monad,Applicative)

instance MonadModule FragnixModule where
    type ModuleInfo FragnixModule = [Symbol]
    lookupInCache name = FragnixModule (do
        let modulname = UnAnn.ModuleName (modToString name)
        gets (Map.lookup modulname))
    insertInCache name symbols = FragnixModule (do
        modify (Map.insert (UnAnn.ModuleName (modToString name)) symbols))
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"
