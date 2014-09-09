{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable,OverloadedStrings #-}
module Fragnix.Resolver where

import Fragnix.Slice

import Language.Haskell.Exts.Annotated (
    parseFile,fromParseResult,Language(Haskell2010),prettyPrint,
    Module,SrcSpanInfo,Decl(FunBind,PatBind),ModuleName)
import qualified Language.Haskell.Exts.Annotated as Name (Name(Ident,Symbol))
import Language.Haskell.Names (
    annotateModule,Scoped(Scoped),NameInfo(GlobalValue,GlobalType,ScopeError),
    OrigName(OrigName),GName(GName),SymValueInfo(SymValue),SymTypeInfo(SymType),
    Error)
import Language.Haskell.Names.Interfaces (evalNamesModuleT,NamesDB)
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName,stringToName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Distribution.HaskellSuite.Packages (getInstalledPackages)
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB))
import Data.Proxy (Proxy(Proxy))

import Data.Hashable (hash)

import Control.Exception (Exception,throwIO)
import Data.Typeable (Typeable)
import Data.Data (Data)

import Data.Map (Map)
import qualified Data.Map as Map (fromList,toList,lookup)

import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Foldable (foldMap,toList)
import Control.Monad (when,guard)
import Data.List (nub)
import Data.Maybe (maybeToList)

data NameErrors = NameErrors [Error SrcSpanInfo]

type TempID = Integer

data Symbol = Symbol Entity OriginalModule UsedName

data Entity = ValueEntity | TypeEntity

resolve :: FilePath -> IO ([Slice],SliceID)
resolve filePath = do
    scopedModule <- resolveNames filePath
    let (tempSlices,boundByMap) = extractSlices scopedModule
        tempSliceMap = sliceMap tempSlices
        slices = map (replaceSliceID (computeHash tempSliceMap)) tempSlices
        mainSliceID = findMainSliceID tempSliceMap boundByMap
    return (slices,mainSliceID)

resolveNames :: FilePath -> IO (Module (Scoped SrcSpanInfo))
resolveNames filePath = do
    packages <- getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB
    originalModule <- fromParseResult <$> parseFile filePath
    scopedModule <- evalNamesModuleT (annotateModule Haskell2010 [] originalModule) packages
    let errors = [scopeError | Scoped (ScopeError scopeError) _ <- concatMap toList (getModuleDecls scopedModule)]
    when (not (null errors)) (throwIO (NameErrors errors))
    return scopedModule   

extractSlices :: Module (Scoped SrcSpanInfo) -> ([Slice],Map Symbol TempID)
extractSlices scopedModule = (tempSlices,boundByMap) where
    modulName = getModuleName scopedModule
    declarations = zip [0..] (getModuleDecls scopedModule)
    boundByMap = Map.fromList (do
        (tempID,declaration) <- declarations
        symbol <- boundSymbols modulName declaration
        return (symbol,tempID))
    tempSlices = do
        (tempID,declaration) <- declarations
        fragment <- maybeToList (declarationFragment declaration)
        let usages = do
                mentioned <- mentionedSymbols declaration
                return (findSymbol boundByMap mentioned)
        return (Slice tempID fragment usages)

sliceMap :: [Slice] -> Map TempID Slice
sliceMap tempSlices = Map.fromList (do
     tempSlice@(Slice tempSliceID _ _) <- tempSlices
     return (tempSliceID,tempSlice))

replaceSliceID :: (TempID -> SliceID) -> Slice -> Slice
replaceSliceID f (Slice tempID fragment usages) = Slice (f tempID) fragment (map (replaceUsageID f) usages)

computeHash :: Map TempID Slice -> TempID -> SliceID
computeHash tempSliceMap tempID = abs (fromIntegral (hash (fragment,usages))) where
    Just (Slice _ fragment tempUsages) = Map.lookup tempID tempSliceMap
    usages = map (replaceUsageID (computeHash tempSliceMap)) tempUsages

findMainSliceID :: Map TempID Slice -> Map Symbol TempID -> SliceID
findMainSliceID tempSliceMap boundByMap = head (do
    (Symbol _ _ (VarId name),tempMainSliceID) <- Map.toList boundByMap
    guard (name == "main")
    return (computeHash tempSliceMap tempMainSliceID))

replaceUsageID :: (TempID -> SliceID) -> Usage -> Usage
replaceUsageID f (Usage qualification usedName (OtherSlice tempID)) =
    (Usage qualification usedName (OtherSlice (f tempID)))
replaceUsageID _ usage = usage

declarationFragment :: Decl (Scoped SrcSpanInfo) -> Maybe Fragment
declarationFragment decl@(FunBind _ _) = Just (Fragment [pack (prettyPrint decl)])
declarationFragment decl@(PatBind _ _ _ _ _) = Just (Fragment [pack (prettyPrint decl)])
declarationFragment _ = Nothing

findSymbol :: Map Symbol TempID -> Symbol -> Usage
findSymbol boundBy symbol@(Symbol _ originalModule usedName) = case Map.lookup symbol boundBy of
    Nothing -> Usage Nothing usedName (Primitive originalModule)
    Just tempID -> Usage Nothing usedName (OtherSlice tempID)

boundSymbols :: (Data l,Eq l) => ModuleName l -> Decl l -> [Symbol]
boundSymbols modulName = map infoToSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> Symbol
infoToSymbol (Left (SymValue (OrigName _ (GName originalModule boundName)) _)) =
    Symbol ValueEntity (pack originalModule) (symbolName ValueEntity boundName)

mentionedSymbols :: Decl (Scoped l) -> [Symbol]
mentionedSymbols = nub . foldMap (externalSymbol . (\(Scoped nameInfo _) -> nameInfo))

externalSymbol :: NameInfo l -> [Symbol]
externalSymbol (GlobalValue (SymValue (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol ValueEntity (pack originalModule) (symbolName ValueEntity mentionedName)]
externalSymbol (GlobalType (SymType (OrigName _ (GName originalModule mentionedName)) _)) =
    [Symbol TypeEntity (pack originalModule) (symbolName TypeEntity mentionedName)]
externalSymbol _ = []

symbolName :: Entity -> String -> UsedName
symbolName ValueEntity s = case stringToName s of
    Name.Ident _ name -> VarId (pack name)
    Name.Symbol _ name -> VarSym (pack name)
symbolName TypeEntity s = case stringToName s of
    Name.Ident _ name -> ConId (pack name)
    Name.Symbol _ name -> ConSym (pack name)

deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity

deriving instance Show NameErrors
deriving instance Typeable NameErrors

instance Exception NameErrors
