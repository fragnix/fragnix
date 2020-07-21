{-# OPTIONS_GHC -optc-DPROFILING #-}
{-# LINE 1 "GHC.Stack.CCS.hsc" #-}
{-# LANGUAGE Trustworthy #-}
{-# LINE 2 "GHC.Stack.CCS.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack.CCS
-- Copyright   :  (c) The University of Glasgow 2011
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Access to GHC's call-stack simulation
--
-- @since 4.5.0.0
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash, NoImplicitPrelude #-}
module GHC.Stack.CCS (
    -- * Call stacks
    currentCallStack,
    whoCreated,

    -- * Internals
    CostCentreStack,
    CostCentre,
    getCurrentCCS,
    getCCSOf,
    clearCCS,
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

import GHC.Base
import GHC.Ptr
import GHC.Foreign as GHC
import GHC.IO.Encoding
import GHC.List ( concatMap, reverse )


{-# LINE 49 "GHC.Stack.CCS.hsc" #-}

{-# LINE 50 "GHC.Stack.CCS.hsc" #-}

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

clearCCS :: IO a -> IO a
clearCCS (IO m) = IO $ \s -> clearCCS# m s

ccsCC :: Ptr CostCentreStack -> IO (Ptr CostCentre)
ccsCC p = ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 69 "GHC.Stack.CCS.hsc" #-}

ccsParent :: Ptr CostCentreStack -> IO (Ptr CostCentreStack)
ccsParent p = ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 72 "GHC.Stack.CCS.hsc" #-}

ccLabel :: Ptr CostCentre -> IO CString
ccLabel p = ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 75 "GHC.Stack.CCS.hsc" #-}

ccModule :: Ptr CostCentre -> IO CString
ccModule p = ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 78 "GHC.Stack.CCS.hsc" #-}

ccSrcSpan :: Ptr CostCentre -> IO CString
ccSrcSpan p = ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 81 "GHC.Stack.CCS.hsc" #-}

-- | Returns a @[String]@ representing the current call stack.  This
-- can be useful for debugging.
--
-- The implementation uses the call-stack simulation maintined by the
-- profiler, so it only works if the program was compiled with @-prof@
-- and contains suitable SCC annotations (e.g. by using @-fprof-auto@).
-- Otherwise, the list returned is likely to be empty or
-- uninformative.
--
-- @since 4.5.0.0
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
-- @since 4.5.0.0
whoCreated :: a -> IO [String]
whoCreated obj = do
  ccs <- getCCSOf obj
  ccsToStrings ccs

renderStack :: [String] -> String
renderStack strs =
  "CallStack (from -prof):" ++ concatMap ("\n  "++) (reverse strs)
