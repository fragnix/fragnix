module Fragnix.Symbols where

import Language.Haskell.Names (Symbol(Value))
import Language.Haskell.Names.Interfaces (readInterface)
import Language.Haskell.Exts (ModuleName(ModuleName),Name(Ident))

import Data.Map (Map,fromList)
import System.FilePath ((</>))
import System.Directory (
    getDirectoryContents,doesFileExist,createDirectoryIfMissing)
import Control.Monad (forM,filterM)

loadSymbols :: FilePath -> IO (Map ModuleName [Symbol])
loadSymbols path = do
    createDirectoryIfMissing True path
    filenames <- getDirectoryContents path
    let pathmodulnames = map (\filename -> (ModuleName filename,path </> filename)) filenames
    existingPathModulNames <- filterM (doesFileExist . snd) pathmodulnames
    fmap fromList (forM existingPathModulNames (\(modulname,modulpath) -> do
        symbols <- readInterface modulpath
        return (modulname,symbols)))

primitiveSymbolsPath :: FilePath
primitiveSymbolsPath = "fragnix" </> "primitive_names"

symbolsPath :: FilePath
symbolsPath = "fragnix" </> "names"

primitiveModules :: [ModuleName]
primitiveModules = map ModuleName ["Control.Applicative","Control.Arrow","Control.Category","Control.Concurrent","Control.Concurrent.Chan","Control.Concurrent.MVar","Control.Concurrent.QSem","Control.Concurrent.QSemN","Control.Exception","Control.Exception.Base","Control.Monad","Control.Monad.Fix","Control.Monad.Instances","Control.Monad.ST","Control.Monad.ST.Imp","Control.Monad.ST.Lazy","Control.Monad.ST.Lazy.Imp","Control.Monad.ST.Lazy.Safe","Control.Monad.ST.Lazy.Unsafe","Control.Monad.ST.Safe","Control.Monad.ST.Strict","Control.Monad.ST.Unsafe","Control.Monad.Zip","Data.Array","Data.Array.Base","Data.Array.IArray","Data.Array.IO","Data.Array.IO.Internals","Data.Array.IO.Safe","Data.Array.MArray","Data.Array.MArray.Safe","Data.Array.ST","Data.Array.ST.Safe","Data.Array.Storable","Data.Array.Storable.Internals","Data.Array.Storable.Safe","Data.Array.Unboxed","Data.Array.Unsafe","Data.Bits","Data.Bool","Data.Char","Data.Complex","Data.Data","Data.Dynamic","Data.Either","Data.Eq","Data.Fixed","Data.Foldable","Data.Function","Data.Functor","Data.IORef","Data.Int","Data.Ix","Data.List","Data.Maybe","Data.Monoid","Data.OldTypeable","Data.OldTypeable.Internal","Data.Ord","Data.Ratio","Data.STRef","Data.STRef.Lazy","Data.STRef.Strict","Data.String","Data.Traversable","Data.Tuple","Data.Typeable","Data.Typeable.Internal","Data.Unique","Data.Version","Data.Word","Debug.Trace","Foreign","Foreign.C","Foreign.C.Error","Foreign.C.String","Foreign.C.Types","Foreign.Concurrent","Foreign.ForeignPtr","Foreign.ForeignPtr.Imp","Foreign.ForeignPtr.Safe","Foreign.ForeignPtr.Unsafe","Foreign.Marshal","Foreign.Marshal.Alloc","Foreign.Marshal.Array","Foreign.Marshal.Error","Foreign.Marshal.Pool","Foreign.Marshal.Safe","Foreign.Marshal.Unsafe","Foreign.Marshal.Utils","Foreign.Ptr","Foreign.Safe","Foreign.StablePtr","Foreign.Storable","GHC.Arr","GHC.Base","GHC.CString","GHC.Char","GHC.Classes","GHC.Conc","GHC.Conc.IO","GHC.Conc.Signal","GHC.Conc.Sync","GHC.ConsoleHandler","GHC.Constants","GHC.Debug","GHC.Desugar","GHC.Enum","GHC.Environment","GHC.Err","GHC.Event","GHC.Event.Array","GHC.Event.Clock","GHC.Event.Control","GHC.Event.EPoll","GHC.Event.IntMap","GHC.Event.Internal","GHC.Event.KQueue","GHC.Event.Manager","GHC.Event.PSQ","GHC.Event.Poll","GHC.Event.Thread","GHC.Event.TimerManager","GHC.Event.Unique","GHC.Exception","GHC.Exts","GHC.Fingerprint","GHC.Fingerprint.Type","GHC.Float","GHC.Float.ConversionUtils","GHC.Float.RealFracMethods","GHC.Foreign","GHC.ForeignPtr","GHC.GHCi","GHC.Generics","GHC.IO","GHC.IO.Buffer","GHC.IO.BufferedIO","GHC.IO.Device","GHC.IO.Encoding","GHC.IO.Encoding.CodePage","GHC.IO.Encoding.Failure","GHC.IO.Encoding.Iconv","GHC.IO.Encoding.Latin1","GHC.IO.Encoding.Types","GHC.IO.Encoding.UTF16","GHC.IO.Encoding.UTF32","GHC.IO.Encoding.UTF8","GHC.IO.Exception","GHC.IO.FD","GHC.IO.Handle","GHC.IO.Handle.FD","GHC.IO.Handle.Internals","GHC.IO.Handle.Text","GHC.IO.Handle.Types","GHC.IO.IOMode","GHC.IOArray","GHC.IORef","GHC.IP","GHC.Int","GHC.IntWord64","GHC.Integer","GHC.Integer.Logarithms","GHC.Integer.Logarithms.Internals","GHC.Integer.Simple.Internals","GHC.Integer.Type","GHC.List","GHC.MVar","GHC.Magic","GHC.Num","GHC.PArr","GHC.Pack","GHC.Prim","GHC.PrimopWrappers","GHC.Profiling","GHC.Ptr","GHC.Read","GHC.Real","GHC.ST","GHC.STRef","GHC.Show","GHC.Stable","GHC.Stack","GHC.Stats","GHC.Storable","GHC.TopHandler","GHC.Tuple","GHC.TypeLits","GHC.Types","GHC.Unicode","GHC.Weak","GHC.Word","Numeric","Prelude","System.CPUTime","System.Console.GetOpt","System.Environment","System.Environment.ExecutablePath","System.Exit","System.IO","System.IO.Error","System.IO.Unsafe","System.Info","System.Mem","System.Mem.StableName","System.Mem.Weak","System.Posix.Internals","System.Posix.Types","System.Timeout","Text.ParserCombinators.ReadP","Text.ParserCombinators.ReadPrec","Text.Printf","Text.Read","Text.Read.Lex","Text.Show","Text.Show.Functions","Unsafe.Coerce"]

mainsymbol :: Symbol
mainsymbol = Value (ModuleName "Main") (Ident "main")
