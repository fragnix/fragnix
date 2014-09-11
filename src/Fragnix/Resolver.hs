{-# LANGUAGE StandaloneDeriving,DeriveDataTypeable,OverloadedStrings #-}
module Fragnix.Resolver where

import Fragnix.Slice

import Language.Haskell.Exts.Annotated (
    parseFile,fromParseResult,Language(Haskell2010),prettyPrint,
    Module,SrcSpanInfo,ModuleName,
    Decl(FunBind,PatBind,TypeDecl,DataDecl,TypeSig))
import qualified Language.Haskell.Exts.Annotated as Name (Name(Ident,Symbol))
import Language.Haskell.Names (
    annotateModule,Scoped(Scoped),NameInfo(GlobalValue,GlobalType,ScopeError),
    OrigName(OrigName),GName(GName),
    SymValueInfo(SymValue,SymMethod,SymSelector,SymConstructor),
    SymTypeInfo(SymType,SymData,SymNewType,SymTypeFam,SymDataFam,SymClass),
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

data NameErrors = NameErrors [Error SrcSpanInfo]

type TempID = Integer

data Symbol = Symbol NameSpace OriginalModule UsedName

data NameSpace = ValueSpace | TypeSpace

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
        guard (relevantDecl declaration)
        let signatures = findSignatures (boundSymbols modulName declaration) (map snd declarations)
            fragmentDeclarations = signatures ++ [declaration]
            usages = do
                fragmentDeclaration <- fragmentDeclarations
                mentioned <- mentionedSymbols fragmentDeclaration
                return (findSymbol boundByMap mentioned)
            fragment = Fragment (map (pack . prettyPrint) fragmentDeclarations)
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
    (Symbol _ _ (ValueIdentifier name),tempMainSliceID) <- Map.toList boundByMap
    guard (name == "main")
    return (computeHash tempSliceMap tempMainSliceID))

replaceUsageID :: (TempID -> SliceID) -> Usage -> Usage
replaceUsageID f (Usage qualification usedName (OtherSlice tempID)) =
    (Usage qualification usedName (OtherSlice (f tempID)))
replaceUsageID _ usage = usage

relevantDecl :: Decl (Scoped SrcSpanInfo) -> Bool
relevantDecl (FunBind _ _) = True
relevantDecl (PatBind _ _ _ _ _) = True
relevantDecl (TypeDecl _ _ _) = True
relevantDecl (DataDecl _ _ _ _ _ _) = True
relevantDecl _ = False

findSymbol :: Map Symbol TempID -> Symbol -> Usage
findSymbol boundBy symbol@(Symbol _ originalModule usedName) = case Map.lookup symbol boundBy of
    Nothing -> Usage Nothing usedName (Primitive originalModule)
    Just tempID -> Usage Nothing usedName (OtherSlice tempID)

boundSymbols :: (Data l,Eq l) => ModuleName l -> Decl l -> [Symbol]
boundSymbols modulName = map infoToSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> Symbol
infoToSymbol = either symValueInfoSymbol symTypeInfoSymbol

mentionedSymbols :: Decl (Scoped l) -> [Symbol]
mentionedSymbols = nub . foldMap (externalSymbol . (\(Scoped nameInfo _) -> nameInfo))

externalSymbol :: NameInfo l -> [Symbol]
externalSymbol (GlobalValue symValueInfo) = [symValueInfoSymbol symValueInfo]
externalSymbol (GlobalType symTypeInfo) = [symTypeInfoSymbol symTypeInfo]
externalSymbol _ = []

symValueInfoSymbol :: SymValueInfo OrigName -> Symbol
symValueInfoSymbol (SymValue  origName _) = valueSymbol origName
symValueInfoSymbol (SymMethod origName _ _) = valueSymbol origName
symValueInfoSymbol (SymSelector origName _ _ _) = valueSymbol origName
symValueInfoSymbol (SymConstructor origName _ origTypeName) =
    Symbol ValueSpace (pack originalModule) constructorName where
        OrigName _ (GName originalModule s) = origName
        OrigName _ (GName _ typeName) = origTypeName
        constructorName = case stringToName s of
            Name.Ident _ name -> ConstructorIdentifier (pack typeName) (pack name)
            Name.Symbol _ name -> ConstructorOperator (pack typeName) (pack name)

symTypeInfoSymbol :: SymTypeInfo OrigName -> Symbol
symTypeInfoSymbol (SymType origName _ ) = typeSymbol origName
symTypeInfoSymbol (SymData origName _ ) = typeSymbol origName
symTypeInfoSymbol (SymNewType origName _ ) = typeSymbol origName
symTypeInfoSymbol (SymTypeFam origName _ ) = typeSymbol origName
symTypeInfoSymbol (SymDataFam origName _ ) = typeSymbol origName
symTypeInfoSymbol (SymClass origName _ ) = typeSymbol origName

valueSymbol :: OrigName -> Symbol
valueSymbol (OrigName _ (GName originalModule name)) =
    Symbol ValueSpace (pack originalModule) (symbolName ValueSpace name)

typeSymbol :: OrigName -> Symbol
typeSymbol (OrigName _ (GName originalModule name)) =
    Symbol TypeSpace (pack originalModule) (symbolName TypeSpace name)

symbolName :: NameSpace -> String -> UsedName
symbolName ValueSpace s = case stringToName s of
    Name.Ident _ name -> ValueIdentifier (pack name)
    Name.Symbol _ name -> ValueOperator (pack name)
symbolName TypeSpace s = case stringToName s of
    Name.Ident _ name -> TypeIdentifier (pack name)
    Name.Symbol _ name -> TypeOperator (pack name)

findSignatures :: [Symbol] -> [Decl (Scoped SrcSpanInfo)] -> [Decl (Scoped SrcSpanInfo)]
findSignatures symbols declarations = do
    TypeSig annotation signatureNames signatureType <- declarations
    signatureName <- signatureNames
    guard (or (do
        Symbol _ _ usedName <- symbols
        case signatureName of
            Name.Ident _ name -> case usedName of
                ValueIdentifier boundName -> return (pack name == boundName)
                _ -> return False
            Name.Symbol _ name -> case usedName of
                ValueOperator boundName -> return (pack name == boundName)
                _ -> return False))
    return (TypeSig annotation [signatureName] signatureType)


deriving instance Show Symbol
deriving instance Eq Symbol
deriving instance Ord Symbol

deriving instance Show NameSpace
deriving instance Eq NameSpace
deriving instance Ord NameSpace

deriving instance Show NameErrors
deriving instance Typeable NameErrors

instance Exception NameErrors
