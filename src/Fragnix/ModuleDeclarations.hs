{-# LANGUAGE TypeFamilies,GeneralizedNewtypeDeriving #-}
module Fragnix.ModuleDeclarations
  ( parse
  , moduleDeclarationsWithEnvironment
  , moduleSymbols
  ) where

import Fragnix.Declaration (
    Declaration(Declaration),Genre(..))
import Fragnix.Environment (
    loadEnvironment,environmentPath,builtinEnvironmentPath)

import Language.Haskell.Exts (
    Module,ModuleName,QName(Qual,UnQual),Decl(..),
    parseFileContentsWithMode,defaultParseMode,ParseMode(..),baseFixities,
    ParseResult(ParseOk,ParseFailed),
    SrcSpan,srcInfoSpan,SrcLoc(SrcLoc),
    prettyPrint,
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
import Data.Text (pack)
import Data.Foldable (toList)

-------------------------------------------------------------
-- NEW IMPORTS
------------------------------------------------------------

import SrcLoc (Located, mkRealSrcLoc)
import HsSyn (HsModule)
import HsExtension (GhcPs)
import FastString (mkFastString)
import StringBuffer (stringToStringBuffer)
import Parser (parseModule)
import Lexer (mkPState, P(..), ParseResult(..))
import DynFlags (DynFlags, Settings(..), defaultDynFlags, PlatformConstants(..))
import Bag (bagToList)
import Platform (Platform(..), Arch(..), OS(..))


-------------------------------------------------------------
-- NEW IMPORTS END
------------------------------------------------------------

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
            allExtensions = moduleExtensions ++ globalExtensions ++ perhapsTemplateHaskell moduleExtensions
        extractDeclarations allExtensions annotatedModule
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
    moduleName <- map (dropAnn . getModuleName) modules
    return (moduleName,environment' Map.! moduleName))


parse :: FilePath -> IO (Module SrcSpan)
parse path = do
    fileContents <- readFile path
    let parseMode = defaultParseMode {
            parseFilename = path,
            extensions = globalExtensions ++ perhapsTemplateHaskell moduleExtensions,
            fixities = Just baseFixities}
        parseresult = parseFileContentsWithMode parseMode fileContents
        moduleExtensions = maybe [] snd (readExtensions fileContents)
    case parseresult of
        ParseOk ast -> return (fmap srcInfoSpan ast)
        ParseFailed (SrcLoc filename line column) message -> error (unlines [
            "failed to parse module.",
            "filename: " ++ filename,
            "line: " ++ show line,
            "column: " ++ show column,
            "error: " ++ message])


-- We need the complete dynflags, even though most of them are completely irrelevant for the frontend.
defDynFlags :: DynFlags
defDynFlags = defaultDynFlags settings llvmconfig
  where
    settings = Settings
      { sTargetPlatform = Platform ArchUnknown OSUnknown 64 False False False False False
      , sPlatformConstants = PlatformConstants { pc_DYNAMIC_BY_DEFAULT = False }
      } -- We hope that the frontend doesn't force the rest of the settings. Otherwise, fill in as needed.
    llvmconfig = ([],[])

parseNEW :: FilePath -> IO (Located (HsModule GhcPs))
parseNEW path = do
  fileContents <- readFile path
  let location = mkRealSrcLoc (mkFastString path) 1 1
  let buffer = stringToStringBuffer fileContents
  let parseState = mkPState defDynFlags buffer location
  case unP parseModule parseState of
    POk _ a -> return a
    PFailed messages _ _ -> do
      let (warnings, errors) = messages defDynFlags
      forM (bagToList warnings) print
      forM (bagToList errors) print
      error $ "Could not parse module: " <> path


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
perhapsTemplateHaskell extensions =
  if any (== UnknownExtension "TemplateHaskellQuotes") extensions
    then [EnableExtension TemplateHaskell]
    else []

extractDeclarations :: [Extension] -> Module (Scoped SrcSpan) -> [Declaration]
extractDeclarations declarationExtensions annotatedast =
    mapMaybe (declToDeclaration declarationExtensions modulnameast) (getModuleDecls annotatedast) where
        modulnameast = getModuleName annotatedast

-- | Make a 'Declaration' from a 'haskell-src-exts' 'Decl'.
declToDeclaration ::
    [Extension] ->
    ModuleName (Scoped SrcSpan) ->
    Decl (Scoped SrcSpan) ->
    Maybe Declaration
declToDeclaration declarationExtensions modulnameast annotatedast = do
    let genre = declGenre annotatedast
    case genre of
        Other -> Nothing
        _ -> return (Declaration
            genre
            declarationExtensions
            (pack (prettyPrint annotatedast))
            (declaredSymbols modulnameast annotatedast)
            (mentionedSymbols annotatedast))

-- | The genre of a declaration, for example Type, Value, TypeSignature, ...
declGenre :: Decl (Scoped SrcSpan) -> Genre
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
declGenre (DerivDecl _ _ _ _) = DerivingInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = ForeignImport
declGenre (InfixDecl _ _ _ _) = InfixFixity
declGenre _ = Other

-- | All symbols the given declaration in a module with the given name binds.
declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> [Symbol]
declaredSymbols modulnameast annotatedast =
  getTopDeclSymbols GlobalTable.empty modulnameast annotatedast

-- | All symbols the given declaration mentions together with a qualifiaction
-- if they are used qualified.
mentionedSymbols :: Decl (Scoped SrcSpan) -> [(Symbol,Maybe (ModuleName ()))]
mentionedSymbols decl = concatMap scopeSymbol (toList decl)

-- | Get all references to global symbols from the given scope annotation.
scopeSymbol :: Scoped SrcSpan -> [(Symbol,Maybe (ModuleName ()))]
scopeSymbol (Scoped (GlobalSymbol symbol (Qual _ modulname _)) _) =
  [(symbol,Just (dropAnn modulname))]
scopeSymbol (Scoped (GlobalSymbol symbol (UnQual _ _)) _) =
  [(symbol,Nothing)]
scopeSymbol (Scoped (RecPatWildcard symbols) _) =
  map (\symbol -> (symbol,Nothing)) symbols
scopeSymbol _ =
  []

