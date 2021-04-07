module Show (showInformation) where

import Data.Text (Text, unpack)
import Data.List (intercalate)
import Network.HTTP.Req
import Fragnix.Slice (Slice(..), Fragment(..), Language (..), Use(..), UsedName(..), Reference(..))
import qualified Fragnix.Slice as F (Name(..)) 
import Language.Haskell.Names (Symbol, symbolName, symbolModule)
import Language.Haskell.Exts.Syntax (ModuleName(..), Name(..))
import Utils (IDType (SliceID, EnvID), sliceRequest, envRequest)

showInformation :: IDType -> IO ()
showInformation (SliceID sliceId) = do
  r <- sliceRequest sliceId
  putStrLn $ prettyPrintSlice (responseBody r :: Slice)
showInformation (EnvID envId) = do
  r <- envRequest envId
  putStrLn $ prettyPrintEnv envId (responseBody r :: [Symbol])


prettyPrintSlice :: Slice -> String
prettyPrintSlice sl = "Slice " ++ unpack (sliceID sl) ++ "\n"
                    ++ "--------------------------\n\n"
                    ++ fragmentToString (fragment sl) ++ "\n\n"
                    ++ "--------------------------\n"
                    ++ "Language Extensions:\n"
                    ++ languageToString (language sl) ++ "\n\n"
                    ++ "Dependencies:\n"
                    ++ usesToString (uses sl)

fragmentToString :: Fragment -> String
fragmentToString (Fragment sc) = intercalate "\n" (map unpack sc)

languageToString :: Language -> String
languageToString (Language exts) = intercalate "\n" (map (\e -> "    " ++ unpack e) exts)

usesToString :: [Use] -> String
usesToString us = intercalate "\n" (map (\e -> "    " ++ useToString e) us)

useToString :: Use -> String
useToString (Use Nothing name ref) = usedNameToString name ++ " from " ++ refToString ref
useToString (Use (Just qual) name ref) = usedNameToString name ++ " as " ++ unpack qual ++ " from " ++ refToString ref

usedNameToString :: UsedName -> String
usedNameToString (ConstructorName _ n) = nameToString n
usedNameToString (ValueName n) = nameToString n
usedNameToString (TypeName n) = nameToString n

nameToString :: F.Name -> String
nameToString (F.Identifier n) = unpack n
nameToString (F.Operator n) = "(" ++ unpack n ++ ")"

refToString :: Reference -> String
refToString (OtherSlice s) = "Slice " ++ unpack s
refToString (Builtin m) = unpack m ++ " (builtin)"


prettyPrintEnv :: Text -> [Symbol] -> String
prettyPrintEnv name symbols = "Environment " ++ unpack name ++ "\n"
                            ++ "----------------------------\n"
                            ++ intercalate "\n" (map symbolToString symbols)

symbolToString :: Symbol -> String
symbolToString s = symbolModuleToString (symbolModule s) ++ ": " ++ symbolNameToString (symbolName s)

symbolNameToString :: Name () -> String
symbolNameToString (Ident () s) = s
symbolNameToString (Symbol () s) = s

symbolModuleToString :: ModuleName () -> String
symbolModuleToString (ModuleName () s) = s