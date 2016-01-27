{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Environment (
    loadEnvironment,environmentPath,builtinEnvironmentPath)

import qualified Language.Haskell.Exts as UnAnn (
    QName(Qual,UnQual),ModuleName)
import Language.Haskell.Exts.Annotated (
    Module,ModuleName,Decl(..),
    parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
    ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,SrcLoc(SrcLoc),
    prettyPrint,
    Extension(EnableExtension),
    KnownExtension(..))
import Language.Haskell.Exts.Annotated.Simplify (
    sModuleName)
import Language.Haskell.Names (
    resolve,annotate,
    Environment,Symbol,Error,Scoped(Scoped),
    NameInfo(GlobalSymbol,RecPatWildcard,ScopeError))
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,getModuleExtensions)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)


import qualified Data.Map.Strict as Map (
    union,(!),fromList)
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Data.Foldable (toList)
import Data.List (isPrefixOf)


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
        return (Declaration genre (moduleExtensions ++ globalExtensions) ast boundsymbols mentionedsymbols)
    environment' = resolve modules environment
    annotatedModules = map (annotate environment') modules


moduleNameErrors :: Environment -> [Module SrcSpan] -> [Error SrcSpan]
moduleNameErrors environment modules = errors where
    errors = do
        Scoped (ScopeError errorInfo) _ <- concatMap toList annotatedModules
        return errorInfo
    annotatedModules = map (annotate environment') modules
    environment' = resolve modules environment


-- | Get the exports of the given modules resolved against the given environment.
moduleSymbols :: Environment -> [Module SrcSpan] -> Environment
moduleSymbols environment modules = Map.fromList (do
    let environment' = resolve modules environment
    moduleName <- map (sModuleName . getModuleName) modules
    return (moduleName,environment' Map.! moduleName))


parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    fileContents <- readFile path
    let parseMode = defaultParseMode {
            parseFilename = path,
            extensions = globalExtensions,
            fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode parseMode (stripRoles fileContents)
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed (SrcLoc filename line column) message -> error (unlines [
            "failed to parse module.",
            "filename: " ++ filename,
            "line: " ++ show line,
            "column: " ++ show column,
            "error: " ++ message])

-- | haskell-src-exts parser is not yet able to parse role annotations.
stripRoles :: String -> String
stripRoles = unlines . map replaceRoleLine . lines where
    replaceRoleLine line
        | "type role" `isPrefixOf` line = ""
        | otherwise = line


globalExtensions :: [Extension]
globalExtensions = [
    EnableExtension MultiParamTypeClasses,
    EnableExtension NondecreasingIndentation,
    EnableExtension ExplicitForAll,
    EnableExtension PatternGuards]

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
declGenre (TypeInsDecl _ _ _) = FamilyInstance
declGenre (DataInsDecl _ _ _ _ _) = FamilyInstance
declGenre (GDataInsDecl _ _ _ _ _ _) = FamilyInstance
declGenre (ClassDecl _ _ _ _ _) = TypeClass
declGenre (InstDecl _ _ _ _) = TypeClassInstance
declGenre (DerivDecl _ _ _) = DerivingInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = ForeignImport
declGenre (InfixDecl _ _ _ _) = InfixFixity
declGenre _ = Other

-- | All symbols the given declaration in a module with the given name binds.
declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> [Symbol]
declaredSymbols modulnameast annotatedast = getTopDeclSymbols GlobalTable.empty modulnameast annotatedast

-- | All symbols the given declaration mentions together with a qualifiaction
-- if they are used qualified. Foreign imports have an implicit dependency on
-- the constructors of all mentioned newtypes. If they are a newtype around
-- another newtype they also have an implicitly dependency on it. This inner
-- newtype is usually 'CInt'. As a hack we just add it.
mentionedSymbols :: Decl (Scoped SrcSpan) -> [(Symbol,Maybe UnAnn.ModuleName)]
mentionedSymbols decl = concatMap scopeSymbol (toList decl)

-- | Get all references to global symbols from the given scope annotation.
scopeSymbol :: Scoped SrcSpan -> [(Symbol,Maybe UnAnn.ModuleName)]
scopeSymbol (Scoped (GlobalSymbol symbol (UnAnn.Qual modulname _)) _) = [(symbol,Just modulname)]
scopeSymbol (Scoped (GlobalSymbol symbol (UnAnn.UnQual _)) _) = [(symbol,Nothing)]
scopeSymbol (Scoped (RecPatWildcard symbols) _) = map (\symbol -> (symbol,Nothing)) symbols
scopeSymbol _ = []

