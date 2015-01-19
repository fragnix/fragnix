{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Environment (
    Environment,
    loadEnvironment,environmentPath,builtinEnvironmentPath)

import qualified Language.Haskell.Exts as UnAnn (
    QName(Qual,UnQual),ModuleName(ModuleName))
import Language.Haskell.Exts.Annotated (
    Module,Decl(..),parseFile,ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,ModuleName,
    prettyPrint,Language(Haskell2010),Extension)
import Language.Haskell.Exts.Annotated.Simplify (
    sModuleName)
import Language.Haskell.Names (
    Symbol(NewType,Constructor),Error,Scoped(Scoped),
    computeInterfaces,annotateModule,
    NameInfo(GlobalSymbol,RecPatWildcard))
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,getModuleExtensions)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)
import Distribution.HaskellSuite.Modules (
    MonadModule(..),ModuleInfo,modToString)


import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (
    lookup,insert,union,(!),fromList)
import Control.Monad.Trans.State.Strict (
    State,execState,evalState,gets,modify)
import Control.Monad (forM)
import Control.Applicative (Applicative)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Data.Foldable (toList)
import Data.List (nub)


-- | Given a list of filepaths to valid Haskell modules produces a list of all
-- declarations in those modules. The default environment loaded and used.
moduleDeclarations :: [FilePath] -> IO [Declaration]
moduleDeclarations modulepaths = do
    builtinEnvironment <- loadEnvironment builtinEnvironmentPath
    environment <- loadEnvironment environmentPath
    modules <- forM modulepaths parse
    return (moduleDeclarationsWithEnvironment (Map.union builtinEnvironment environment) modules)


-- | Use the given environment to produce a list of all declarations from the given list
-- of modules.
moduleDeclarationsWithEnvironment :: Environment -> [Module SrcSpan] -> [Declaration]
moduleDeclarationsWithEnvironment environment modules = declarations where
    declarations = do
        annotatedModule <- annotatedModules
        let (_,moduleExtensions) = getModuleExtensions annotatedModule
        Declaration genre _ ast boundsymbols mentionedsymbols <- extractDeclarations annotatedModule
        return (Declaration genre moduleExtensions ast boundsymbols mentionedsymbols)
    annotatedModules = flip evalState environment (do
        resolve modules
        forM modules annotate)


-- | Get the exports of the given modules resolved against the given environment.
moduleSymbols :: Environment -> [Module SrcSpan] -> Environment
moduleSymbols environment modules = Map.fromList (do
    let environment' = execState (resolve modules) environment
    moduleName <- map (sModuleName . getModuleName) modules
    return (moduleName,environment' Map.! moduleName))


parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    parseresult <- parseFile path
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed location message -> error ("PARSE FAILED: " ++ path ++ show location ++ message)

resolve :: [Module SrcSpan] -> State Environment (Set (Error SrcSpan))
resolve asts = runFragnixModule (computeInterfaces language extensions asts)

annotate :: Module SrcSpan -> State Environment (Module (Scoped SrcSpan))
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
