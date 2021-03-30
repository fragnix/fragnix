module Fragnix.FragnixModule where

import qualified Data.Text as T
import qualified Language.Haskell.Names as N
import qualified Language.Haskell.Names.Environment as NE
import qualified Language.Haskell.Exts as E
import qualified Fragnix.Core.FragnixModule as F



-- Isomorphism between Names

coreNameToName :: F.Name -> E.Name ()
coreNameToName (F.Ident t)  = E.Ident () (T.unpack t)
coreNameToName (F.Symbol t) = E.Symbol () (T.unpack t)

nameToCoreName :: E.Name () -> F.Name
nameToCoreName (E.Ident () s)  = F.Ident (T.pack s)
nameToCoreName (E.Symbol () s) = F.Symbol (T.pack s)

-- Isomorphism between ModuleNames

coreModuleNameToModuleName :: F.ModuleName -> E.ModuleName ()
coreModuleNameToModuleName (F.ModuleName t) = E.ModuleName () (T.unpack t)

moduleNameToCoreModuleName :: E.ModuleName () -> F.ModuleName
moduleNameToCoreModuleName (E.ModuleName () s) = F.ModuleName (T.pack s)

-- Isomorphism between Symbols

coreSymbolToSymbol :: F.Symbol -> N.Symbol
coreSymbolToSymbol (F.Value m n) =
  N.Value (coreModuleNameToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Method m n1 n2) =
  N.Method (coreModuleNameToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Selector m n1 n2 ns) =
  N.Selector (coreModuleNameToModuleName m) (coreNameToName n1) (coreNameToName n2) (coreNameToName <$> ns)
coreSymbolToSymbol (F.Constructor m n1 n2) =
  N.Constructor (coreModuleNameToModuleName m) (coreNameToName n1) (coreNameToName n2)
coreSymbolToSymbol (F.Type m n) =
  N.Type (coreModuleNameToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.Data m n) =
  N.Data (coreModuleNameToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.NewType m n) =
  N.NewType (coreModuleNameToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.TypeFam m n mn) =
  N.TypeFam (coreModuleNameToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.DataFam m n mn) =
  N.DataFam (coreModuleNameToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.Class m n) =
  N.Class (coreModuleNameToModuleName m) (coreNameToName n)
coreSymbolToSymbol (F.PatternConstructor m n mn) =
  N.PatternConstructor (coreModuleNameToModuleName m) (coreNameToName n) (coreNameToName <$> mn)
coreSymbolToSymbol (F.PatternSelector m n1 mn n2) =
  N.PatternSelector (coreModuleNameToModuleName m) (coreNameToName n1) (coreNameToName <$> mn) (coreNameToName n2)

symbolToCoreSymbol :: N.Symbol -> F.Symbol
symbolToCoreSymbol (N.Value m n) =
  F.Value (moduleNameToCoreModuleName m) (nameToCoreName n)
symbolToCoreSymbol (N.Method m n1 n2) =
  F.Method (moduleNameToCoreModuleName m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Selector m n1 n2 ns) =
  F.Selector (moduleNameToCoreModuleName m) (nameToCoreName n1) (nameToCoreName n2) (nameToCoreName <$> ns)
symbolToCoreSymbol (N.Constructor m n1 n2) =
  F.Constructor (moduleNameToCoreModuleName m) (nameToCoreName n1) (nameToCoreName n2)
symbolToCoreSymbol (N.Type m n) =
  F.Type (moduleNameToCoreModuleName m) (nameToCoreName n)
symbolToCoreSymbol (N.Data m n) =
  F.Data (moduleNameToCoreModuleName m) (nameToCoreName n)
symbolToCoreSymbol (N.NewType m n) =
  F.NewType (moduleNameToCoreModuleName m) (nameToCoreName n)
symbolToCoreSymbol (N.TypeFam m n mn) =
  F.TypeFam (moduleNameToCoreModuleName m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.DataFam m n mn) =
  F.DataFam (moduleNameToCoreModuleName m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.Class m n) =
  F.Class (moduleNameToCoreModuleName m) (nameToCoreName n)
symbolToCoreSymbol (N.PatternConstructor m n mn) =
  F.PatternConstructor (moduleNameToCoreModuleName m) (nameToCoreName n) (nameToCoreName <$> mn)
symbolToCoreSymbol (N.PatternSelector m n1 mn n2) =
  F.PatternSelector (moduleNameToCoreModuleName m) (nameToCoreName n1) (nameToCoreName <$> mn) (nameToCoreName n2)

-- Isomorphism between list of FragnixModules and Environment

envToModules :: NE.Environment -> [F.FragnixModule]
envToModules = undefined

modulesToEnv :: [F.FragnixModule] -> NE.Environment
modulesToEnv = undefined
