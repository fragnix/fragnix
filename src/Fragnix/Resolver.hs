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
import qualified Data.Map as Map (fromList,map,toList,lookup,keys)

import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Foldable (foldMap,toList)
import Control.Monad (when,guard)
import Data.List (nub)

data NameErrors = NameErrors [Error SrcSpanInfo]

deriving instance Show NameErrors
deriving instance Typeable NameErrors

instance Exception NameErrors

resolve :: FilePath -> IO ([Slice],SliceID)
resolve filePath = do
    scopedModule <- resolveNames filePath
    let (tempSlices,boundByMap) = extractSlices scopedModule
        tempSliceMap = sliceMap tempSlices
        slices = map (computeHash tempSliceMap) tempSlices
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
    declarationMap = declarations (getModuleDecls scopedModule)
    boundMap = Map.map (boundSymbols modulName) declarationMap
    boundByMap = transposeMap boundMap
    mentionedMap = Map.map mentionedSymbols declarationMap
    fragmentMap = Map.map prettyPrint declarationMap
    usagesMap = Map.map (map (findSymbol boundByMap)) mentionedMap
    tempSlices = do
        key <- Map.keys fragmentMap
        let Just source = Map.lookup key fragmentMap
            Just usages = Map.lookup key usagesMap
        return (Slice key (Fragment [pack source]) usages)

sliceMap :: [Slice] -> Map TempID Slice
sliceMap tempSlices = Map.fromList (do
     tempSlice@(Slice tempSliceID _ _) <- tempSlices
     return (tempSliceID,tempSlice))

computeHash :: Map TempID Slice -> Slice -> Slice
computeHash tempSliceMap (Slice _ fragment tempUsages) = Slice sliceID fragment usages where
    sliceID = fromIntegral (hash (fragment,usages))
    usages = map f tempUsages
    f (Usage qualification usedName (OtherSlice tempID)) = (Usage qualification usedName (OtherSlice otherSliceID)) where
        Just tempSlice = Map.lookup tempID tempSliceMap
        Slice otherSliceID _ _ = computeHash tempSliceMap tempSlice
    f usage = usage

replaceUsageID :: Map TempID SliceID -> Usage -> Maybe Usage
replaceUsageID sliceIDMap (Usage qualification usedName (OtherSlice tempID)) = do
    sliceID <- Map.lookup tempID sliceIDMap
    return (Usage qualification usedName (OtherSlice sliceID))
replaceUsageID _ usage = Just usage

findMainSliceID :: Map TempID Slice -> Map Symbol TempID -> SliceID
findMainSliceID tempSliceMap boundByMap = head (do
    (Symbol _ _ (VarId name),tempMainSliceID) <- Map.toList boundByMap
    guard (name == "main")
    let Just tempMainSlice = Map.lookup tempMainSliceID tempSliceMap
        Slice mainSliceID _ _ = computeHash tempSliceMap tempMainSlice
    return mainSliceID)
        

type TempID = Integer

data Symbol = Symbol Entity OriginalModule UsedName

deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol

data Entity = ValueEntity | TypeEntity

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity

declarations :: [Decl l] -> Map TempID (Decl l)
declarations = Map.fromList . zip [0..] . concatMap binding

binding :: Decl l -> [Decl l]
binding decl@(FunBind _ _) = [decl]
binding decl@(PatBind _ _ _ _ _) = [decl]
binding _ = []

findSymbol :: Map Symbol TempID -> Symbol -> Usage
findSymbol boundBy symbol@(Symbol _ originalModule usedName) = case Map.lookup symbol boundBy of
    Nothing -> Usage Nothing usedName (Primitive originalModule)
    Just tempID -> Usage Nothing usedName (OtherSlice tempID)

boundSymbols :: (Data l,Eq l) => ModuleName l -> Decl l -> [Symbol]
boundSymbols modulName = map infoToSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> Symbol
infoToSymbol (Left (SymValue (OrigName _ (GName originalModule boundName)) _)) =
    Symbol ValueEntity (pack originalModule) (symbolName ValueEntity boundName)

transposeMap :: (Ord v) => Map k [v] -> Map v k
transposeMap = Map.fromList . concatMap (\(k,vs) -> [(v,k) | v <- vs]) . Map.toList

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
