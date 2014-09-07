{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable #-}
module Fragnix.Resolver where

import Fragnix.Slice

import Language.Haskell.Exts.Annotated (
    parseFile,fromParseResult,Language(Haskell2010),prettyPrint,
    Module(Module),SrcSpanInfo,Decl,ModuleName)
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

import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Foldable (foldMap,toList)
import Control.Monad (when,void)

data NameErrors = NameErrors [Error ()]

deriving instance Show NameErrors
deriving instance Typeable NameErrors

instance Exception NameErrors

extractSlices :: FilePath -> IO [Slice]
extractSlices filePath = do
    packages <- getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
    originalModule <- fromParseResult <$> parseFile filePath
    scopedModule <- evalNamesModuleT (annotateModule Haskell2010 [] originalModule) packages
    let nameInfoModule = fmap (\(Scoped nameInfo _) -> void nameInfo) scopedModule
        modulName = getModuleName nameInfoModule
        decls = getModuleDecls nameInfoModule
        errors = concatMap scopeErrors decls
--    when (not (null errors)) (throwIO (NameErrors errors))
    print errors
    print (map (boundSymbols modulName) decls)
    print (map prettyPrint (getModuleDecls scopedModule))
    print (map extractMentionedSymbols decls)
    return []

data Symbol = Symbol Entity OriginalModule UsedName

deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol

data Entity = ValueEntity | TypeEntity

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity

boundSymbols :: ModuleName (NameInfo ()) -> Decl (NameInfo ()) -> [Symbol]
boundSymbols modulName = map infoToSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> Symbol
infoToSymbol (Left (SymValue (OrigName _ (GName originalModule boundName)) _)) =
    Symbol ValueEntity (pack originalModule) (VarId (pack boundName))

scopeErrors :: Decl (NameInfo ()) -> [Error ()]
scopeErrors decl = [scopeError | ScopeError scopeError <- toList decl]

extractMentionedSymbols :: Decl (NameInfo ()) -> [Symbol]
extractMentionedSymbols = foldMap externalSymbol

externalSymbol :: NameInfo () -> [Symbol]
externalSymbol (GlobalValue (SymValue (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol ValueEntity (pack originalModule) (VarId (pack mentionedName))]
externalSymbol (GlobalType (SymType (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol TypeEntity (pack originalModule) (ConId (pack mentionedName))]
externalSymbol _ = []
