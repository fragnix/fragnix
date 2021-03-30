module Fragnix.FragnixModule where

import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Names.Environment as NE
import qualified Language.Haskell.Exts as E
import qualified Fragnix.Core.FragnixModule as F
import qualified Fragnix.Core.Slice as S



-- Isomorphism between Names

coreNameToName :: F.Name -> E.Name ()
coreNameToName (F.Ident t)  = E.Ident () (T.unpack t)
coreNameToName (F.Symbol t) = E.Symbol () (T.unpack t)

nameToCoreName :: E.Name () -> F.Name
nameToCoreName (E.Ident () s)  = F.Ident (T.pack s)
nameToCoreName (E.Symbol () s) = F.Symbol (T.pack s)

-- Isomorphism between ModuleName and Reference

referenceToModuleName :: S.Reference -> E.ModuleName ()
referenceToModuleName (S.OtherSlice t) = E.ModuleName () ("F" <> T.unpack t)
referenceToModuleName (S.Builtin t) = E.ModuleName () (T.unpack t)

moduleNameToReference :: E.ModuleName () -> S.Reference
moduleNameToReference (E.ModuleName () ('F':rest)) | all isDigit rest = S.OtherSlice (T.pack rest)
moduleNameToReference (E.ModuleName () moduleName) = S.Builtin (T.pack moduleName)

-- Isomorphism between Symbols

coreSymbolToSymbol :: F.Symbol -> N.Symbol
coreSymbolToSymbol (F.Value m n) =
  N.Value (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Method m n1 n2) =
  N.Method (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Selector m n1 n2 ns) =
  N.Selector (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2) (coreNameToName <$> ns)
coreSymbolToSymbol (F.Constructor m n1 n2) =
  N.Constructor (referenceToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Type m n) =
  N.Type (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Data m n) =
  N.Data (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.NewType m n) =
  N.NewType (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.TypeFam m n mn) =
  N.TypeFam (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.DataFam m n mn) =
  N.DataFam (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.Class m n) =
  N.Class (referenceToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.PatternConstructor m n mn) =
  N.PatternConstructor (referenceToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.PatternSelector m n1 mn n2) =
  N.PatternSelector (referenceToModuleName m) (coreNameToName n1) (coreNameToName <$> mn) (coreNameToName n2)

symbolToCoreSymbol :: N.Symbol -> F.Symbol
symbolToCoreSymbol (N.Value m n) =
  F.Value (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.Method m n1 n2) =
  F.Method (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Selector m n1 n2 ns) =
  F.Selector (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2) (nameToCoreName <$> ns)
symbolToCoreSymbol (N.Constructor m n1 n2) =
  F.Constructor (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Type m n) =
  F.Type (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.Data m n) =
  F.Data (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.NewType m n) =
  F.NewType (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.TypeFam m n mn) =
  F.TypeFam (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.DataFam m n mn) =
  F.DataFam (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.Class m n) =
  F.Class (moduleNameToReference m) (nameToCoreName n)
symbolToCoreSymbol (N.PatternConstructor m n mn) =
  F.PatternConstructor (moduleNameToReference m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.PatternSelector m n1 mn n2) =
  F.PatternSelector (moduleNameToReference m) (nameToCoreName n1) (nameToCoreName <$> mn) (nameToCoreName n2)

-- Isomorphism between list of FragnixModules and Environment

envToModules :: NE.Environment -> [F.FragnixModule]
envToModules = undefined

modulesToEnv :: [F.FragnixModule] -> NE.Environment
modulesToEnv = undefined
