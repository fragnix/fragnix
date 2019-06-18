{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving,DeriveDataTypeable,DeriveGeneric #-}
module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Environment (
    loadEnvironment,environmentPath,builtinEnvironmentPath)

import Language.Haskell.Exts (
    Module(Module),ModuleName,QName(Qual,UnQual),Decl(..),
    parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
    ParseResult(ParseOk,ParseFailed),
    SrcSpan(srcSpanStartLine, srcSpanEndLine),srcInfoSpan,SrcLoc(SrcLoc),SrcInfo(..),
    readExtensions,Extension(EnableExtension,UnknownExtension),KnownExtension(..))
import Language.Haskell.Names (
    resolve,annotate,
    Environment,Symbol,Error,Scoped(Scoped),
    NameInfo(GlobalSymbol,RecPatWildcard,ScopeError))
import Language.Haskell.Names.SyntaxUtils (
    getModuleDecls,getModuleName,getModuleExtensions,dropAnn)
import Language.Haskell.Names.ModuleSymbols (
    getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (
    empty)


import qualified Data.Map.Strict as Map (
    union,(!),fromList)
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Monoid (First(First))
import Data.Text (pack)
import Data.Foldable (toList)
import Data.Data (Data)
import GHC.Generics (Generic)


-- | Given a list of filepaths to valid Haskell modules produces a list of all
-- declarations in those modules. The default environment loaded and used.
moduleDeclarations :: [FilePath] -> IO [Declaration]
moduleDeclarations modulepaths = do
    builtinEnvironment <- loadEnvironment builtinEnvironmentPath
    environment <- loadEnvironment environmentPath
    modules <- forM modulepaths parse
    let declarations = moduleDeclarationsWithEnvironment (Map.union builtinEnvironment environment) modules
    return declarations

-- | A source span paired with the source code for top level declarations.
data Source = Source { sourceCode :: Maybe String, sourceSrcSpan :: SrcSpan }
  deriving (Eq, Data, Generic)

instance SrcInfo Source where
  toSrcInfo st ps end = Source Nothing (toSrcInfo st ps end)
  fromSrcInfo = Source Nothing . fromSrcInfo
  fileName = fileName . sourceSrcSpan
  startLine = startLine . sourceSrcSpan
  startColumn = startColumn . sourceSrcSpan


-- | Use the given environment to produce a list of all declarations from the given list
-- of modules.
moduleDeclarationsWithEnvironment :: Environment -> [Module Source] -> [Declaration]
moduleDeclarationsWithEnvironment environment modules = let
    environment' = resolve modules environment
    annotatedModules = map (annotate environment') modules
    declarations = do
        annotatedModule <- annotatedModules
        let (_,moduleExtensions) = getModuleExtensions annotatedModule
        let allExtensions = moduleExtensions ++ globalExtensions ++ perhapsTemplateHaskell moduleExtensions
        extractDeclarations allExtensions annotatedModule
    in declarations


moduleNameErrors :: Environment -> [Module SrcSpan] -> [Error SrcSpan]
moduleNameErrors environment modules = errors where
    errors = do
        Scoped (ScopeError errorInfo) _ <- concatMap toList annotatedModules
        return errorInfo
    annotatedModules = map (annotate environment') modules
    environment' = resolve modules environment


-- | Get the exports of the given modules resolved against the given environment.
moduleSymbols :: (Eq a, Data a) => Environment -> [Module a] -> Environment
moduleSymbols environment modules = Map.fromList (do
    let environment' = resolve modules environment
    moduleName <- map (dropAnn . getModuleName) modules
    return (moduleName,environment' Map.! moduleName))


parse :: FilePath -> IO (Module Source)
parse path = do
    moduleSource <- readFile path
    let parseMode = defaultParseMode {
            parseFilename = path,
            extensions = globalExtensions ++ perhapsTemplateHaskell moduleExtensions,
            fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode parseMode moduleSource
        moduleExtensions = maybe [] snd (readExtensions moduleSource)
    case parseresult of
        ParseOk ast -> return (addTopLevelSource moduleSource (fmap srcInfoSpan ast))
        ParseFailed (SrcLoc filename line column) message -> error (unlines [
            "failed to parse module.",
            "filename: " ++ filename,
            "line: " ++ show line,
            "column: " ++ show column,
            "error: " ++ message])

globalExtensions :: [Extension]
globalExtensions = [
    EnableExtension MultiParamTypeClasses,
    EnableExtension NondecreasingIndentation,
    EnableExtension ExplicitForAll,
    EnableExtension PatternGuards]

-- | Because haskell-src-exts cannot handle TemplateHaskellQuotes we enable
-- TemplateHaskell when we encounter it.
-- See https://github.com/haskell-suite/haskell-src-exts/issues/357
perhapsTemplateHaskell :: [Extension] -> [Extension]
perhapsTemplateHaskell moduleExtensions =
  if any (== UnknownExtension "TemplateHaskellQuotes") moduleExtensions
    then [EnableExtension TemplateHaskell]
    else []

-- | For each top level declaration, add its original source code.
addTopLevelSource :: String -> Module SrcSpan -> Module Source
addTopLevelSource moduleSource modul = case fmap (Source Nothing) modul of
  Module a mh ps is decls -> Module a mh ps is (map (addDeclSource moduleSource) decls)
  _ -> error "Unsupported kind of module"

-- | The original source code of the parsed declaration.
addDeclSource :: String -> Decl Source -> Decl Source
addDeclSource moduleSource declAst = let
    firstLine = minimum (map (srcSpanStartLine . sourceSrcSpan) (toList declAst))
    lastLine = maximum (map (srcSpanEndLine . sourceSrcSpan) (toList declAst))
    -- BUG in HSE: extent of instance declarations is too large
    adjustedLastLine = case declGenre declAst of
      TypeClassInstance -> lastLine - 1
      _ -> lastLine
    -- line count starts at 1
    source = unlines (drop (firstLine - 1) (take adjustedLastLine (lines moduleSource)))
    in fmap (\(Source _ srcSpan) -> Source (Just source) srcSpan) declAst


extractDeclarations :: [Extension] -> Module (Scoped Source) -> [Declaration]
extractDeclarations declarationExtensions annotatedast =
    mapMaybe (declToDeclaration declarationExtensions modulnameast) (getModuleDecls annotatedast) where
        modulnameast = getModuleName annotatedast

-- | Make a 'Declaration' from a 'haskell-src-exts' 'Decl'.
declToDeclaration ::
    [Extension] ->
    ModuleName (Scoped Source) ->
    Decl (Scoped Source) ->
    Maybe Declaration
declToDeclaration declarationExtensions modulnameast annotatedast = do
    let genre = declGenre annotatedast
    case genre of
        Other -> Nothing
        _ -> Just (Declaration
            genre
            declarationExtensions
            (pack (getDeclSource annotatedast))
            (declaredSymbols modulnameast annotatedast)
            (mentionedSymbols annotatedast))

-- | The genre of a declaration, for example Type, Value, TypeSignature, ...
declGenre :: Decl a -> Genre
declGenre (TypeDecl _ _ _) = Type
declGenre (TypeFamDecl _ _ _ _) = Type
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

-- | The original source code of this declaration.
getDeclSource :: Decl (Scoped Source) -> String
getDeclSource declAst = case foldMap (\(Scoped _ (Source source _)) -> First source) declAst of
  First (Just source) -> source
  First Nothing -> error "Source code missing in Decl"

-- | All symbols the given declaration in a module with the given name binds.
declaredSymbols :: (Eq a, Data a) => ModuleName (Scoped a) -> Decl (Scoped a) -> [Symbol]
declaredSymbols modulnameast annotatedast =
  getTopDeclSymbols GlobalTable.empty modulnameast annotatedast

-- | All symbols the given declaration mentions together with a qualifiaction
-- if they are used qualified.
mentionedSymbols :: Decl (Scoped a) -> [(Symbol,Maybe (ModuleName ()))]
mentionedSymbols decl = concatMap scopeSymbol (toList decl)

-- | Get all references to global symbols from the given scope annotation.
scopeSymbol :: Scoped a -> [(Symbol,Maybe (ModuleName ()))]
scopeSymbol (Scoped (GlobalSymbol symbol (Qual _ modulname _)) _) =
  [(symbol,Just (dropAnn modulname))]
scopeSymbol (Scoped (GlobalSymbol symbol (UnQual _ _)) _) =
  [(symbol,Nothing)]
scopeSymbol (Scoped (RecPatWildcard symbols) _) =
  map (\symbol -> (symbol,Nothing)) symbols
scopeSymbol _ =
  []

