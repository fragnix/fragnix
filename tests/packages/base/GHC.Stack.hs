{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/GHC/Stack.hs" #-}
{-# OPTIONS_GHC -optc-DPROFILING #-}
{-# LINE 1 "GHC/Stack.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "GHC/Stack.hsc" #-}
-- |
-- Module      :  GHC.Stack
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- /Since: 4.5.0.0/
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash #-}
module GHC.Stack (
    -- * Call stack
    currentCallStack,
    whoCreated,
    errorWithStackTrace,

    -- * Internals
    CostCentreStack,
    CostCentre,
    getCurrentCCS,
    getCCSOf,
    ccsCC,
    ccsParent,
    ccLabel,
    ccModule,
    ccSrcSpan,
    ccsToStrings,
    renderStack
  ) where

import Foreign
import Foreign.C

import GHC.IO
import GHC.Base
import GHC.Ptr
import GHC.Foreign as GHC
import GHC.IO.Encoding
import GHC.Exception


{-# LINE 48 "GHC/Stack.hsc" #-}

{-# LINE 49 "GHC/Stack.hsc" #-}

data CostCentreStack
data CostCentre

getCurrentCCS :: dummy -> IO (Ptr CostCentreStack)
getCurrentCCS dummy = IO $ \s ->
   case getCurrentCCS# dummy s of
     (# s', addr #) -> (# s', Ptr addr #)

getCCSOf :: a -> IO (Ptr CostCentreStack)
getCCSOf obj = IO $ \s ->
   case getCCSOf# obj s of
     (# s', addr #) -> (# s', Ptr addr #)

ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 65 "GHC/Stack.hsc" #-}

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 68 "GHC/Stack.hsc" #-}

ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 71 "GHC/Stack.hsc" #-}

ccModule :: Ptr CostCentre -> IO CString
ccModule p = ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 74 "GHC/Stack.hsc" #-}

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 77 "GHC/Stack.hsc" #-}

-- | returns a '[String]' representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintined by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- /Since: 4.5.0.0/

currentCallStack :: IO [String]
currentCallStack = ccsToStrings =<< getCurrentCCS ()

ccsToStrings :: Ptr CostCentreStack -> IO [String]
ccsToStrings ccs0 = go ccs0 []
  where
    go ccs acc
     | ccs == nullPtr = return acc
     | otherwise = do
        cc  <- ccsCC ccs
        lbl <- GHC.peekCString utf8 =<< ccLabel cc
        mdl <- GHC.peekCString utf8 =<< ccModule cc
        loc <- GHC.peekCString utf8 =<< ccSrcSpan cc
        parent <- ccsParent ccs
        if (mdl == "MAIN" && lbl == "MAIN")
           then return acc
           else go parent ((mdl ++ '.':lbl ++ ' ':'(':loc ++ ")") : acc)

-- | Get the stack trace attached to an object.
--
-- /Since: 4.5.0.0/
whoCreated :: a -> IO [String]
whoCreated obj = do
  ccs <- getCCSOf obj
  ccsToStrings ccs

renderStack :: [String] -> String
renderStack strs = "Stack trace:" ++ concatMap ("\n  "++) (reverse strs)

-- | Like the function 'error', but appends a stack trace to the error
-- message if one is available.
--
-- /Since: 4.7.0.0/
errorWithStackTrace :: String -> a
errorWithStackTrace x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwIO (ErrorCall x)
      else throwIO (ErrorCall (x ++ '\n' : renderStack stack))
