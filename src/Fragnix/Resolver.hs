{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable,OverloadedStrings #-}
module Fragnix.Resolver where

import Fragnix.Slice

import Language.Haskell.Exts.Annotated (
    parseFile,fromParseResult,Language(Haskell2010),prettyPrint,
    SrcSpanInfo,Decl(FunBind,PatBind),ModuleName)
import Language.Haskell.Names (
    annotateModule,Scoped(Scoped),NameInfo(GlobalValue,GlobalType,ScopeError),
    OrigName(OrigName),GName(GName),SymValueInfo(SymValue),SymTypeInfo(SymType),
    Error)
import Language.Haskell.Names.Interfaces (evalNamesModuleT,NamesDB)
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Distribution.HaskellSuite.Packages (getInstalledPackages)
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB))
import Data.Proxy (Proxy(Proxy))

import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Data.Map (Map)
import qualified Data.Map as Map (fromList,map,toList,lookup,keys)

import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Foldable (foldMap,toList)
import Control.Monad (when)

data NameErrors = NameErrors [Error SrcSpanInfo]

deriving instance Show NameErrors
deriving instance Typeable NameErrors

instance Exception NameErrors

extractSlices :: FilePath -> IO [Slice]
extractSlices filePath = do
    packages <- getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
    originalModule <- fromParseResult <$> parseFile filePath
    scopedModule <- evalNamesModuleT (annotateModule Haskell2010 [] originalModule) packages
    let decls = getModuleDecls scopedModule
        errors = concatMap scopeErrors decls
        modulName = getModuleName scopedModule
        declarations = declarationMap decls
        bound = Map.map (boundSymbols modulName) declarations
        boundBy = Map.fromList (concatMap (\(tempID,symbols) -> [(symbol,tempID) | symbol <- symbols]) (Map.toList bound))
        mentioned = Map.map mentionedSymbols declarations
        fragments = Map.map prettyPrint declarations
        usagess = Map.map (map (resolve boundBy)) mentioned
    when (not (null errors)) (throwIO (NameErrors errors))
    return (do
        key <- Map.keys fragments
        fragment <- maybe [] (return . (Binding "") . pack) (Map.lookup key fragments)
        usages <- maybe [] return (Map.lookup key usagess)
        return (Slice key fragment usages))

declarationMap :: [Decl l] -> Map TempID (Decl l)
declarationMap = Map.fromList . zip [0..] . concatMap binding

binding :: Decl l -> [Decl l]
binding decl@(FunBind _ _) = [decl]
binding decl@(PatBind _ _ _ _ _) = [decl]
binding _ = []

resolve :: Map Symbol TempID -> Symbol -> Usage
resolve boundBy symbol@(Symbol _ originalModule usedName) = case Map.lookup symbol boundBy of
    Nothing -> Usage Nothing usedName (Primitive originalModule)
    Just tempID -> Usage Nothing usedName (OtherSlice tempID)

type TempID = Integer

data Symbol = Symbol Entity OriginalModule UsedName

deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol

data Entity = ValueEntity | TypeEntity

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity

boundSymbols :: (Data l,Eq l) => ModuleName l -> Decl l -> [Symbol]
boundSymbols modulName = map infoToSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> Symbol
infoToSymbol (Left (SymValue (OrigName _ (GName originalModule boundName)) _)) =
    Symbol ValueEntity (pack originalModule) (VarId (pack boundName))

scopeErrors :: Decl (Scoped l) -> [Error l]
scopeErrors decl = [scopeError | Scoped (ScopeError scopeError) _ <- toList decl]

mentionedSymbols :: Decl (Scoped l) -> [Symbol]
mentionedSymbols = foldMap (externalSymbol . (\(Scoped nameInfo _) -> nameInfo))

externalSymbol :: NameInfo l -> [Symbol]
externalSymbol (GlobalValue (SymValue (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol ValueEntity (pack originalModule) (VarId (pack mentionedName))]
externalSymbol (GlobalType (SymType (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol TypeEntity (pack originalModule) (ConId (pack mentionedName))]
externalSymbol _ = []
