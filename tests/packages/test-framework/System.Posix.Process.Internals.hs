{-# LINE 1 "./System/Posix/Process/Internals.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./System/Posix/Process/Internals.hs" #-}
{-# LINE 1 "./System/Posix/Process/Internals.hs" #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE Trustworthy #-}


module System.Posix.Process.Internals (
       pPrPr_disableITimers, c_execvpe,
       decipherWaitStatus, ProcessStatus(..) ) where

import Foreign
import Foreign.C
import System.Exit
import System.IO.Error
import GHC.Conc (Signal)

-- | The exit status of a process
data ProcessStatus
   = Exited ExitCode        -- ^ the process exited by calling
                            -- @exit()@ or returning from @main@
   | Terminated Signal Bool -- ^ the process was terminated by a
                            -- signal, the @Bool@ is @True@ if a core
                            -- dump was produced
                            --
                            -- /Since: 2.7.0.0/
   | Stopped Signal         -- ^ the process was stopped by a signal
   deriving (Eq, Ord, Show)

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt

decipherWaitStatus :: CInt -> IO ProcessStatus
decipherWaitStatus wstat =
  if c_WIFEXITED wstat /= 0
      then do
        let exitstatus = c_WEXITSTATUS wstat
        if exitstatus == 0
	   then return (Exited ExitSuccess)
	   else return (Exited (ExitFailure (fromIntegral exitstatus)))
      else do
        if c_WIFSIGNALED wstat /= 0
	   then do
                let termsig    = c_WTERMSIG wstat
                let coredumped = c_WCOREDUMP wstat /= 0
                return (Terminated termsig coredumped)
	   else do
		if c_WIFSTOPPED wstat /= 0
		   then do
			let stopsig = c_WSTOPSIG wstat
                        return (Stopped stopsig)
		   else do
			ioError (mkIOError illegalOperationErrorType
				   "waitStatus" Nothing Nothing)

foreign import ccall unsafe "__hsunix_wifexited"
  c_WIFEXITED :: CInt -> CInt 

foreign import ccall unsafe "__hsunix_wexitstatus"
  c_WEXITSTATUS :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wifsignaled"
  c_WIFSIGNALED :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wtermsig"
  c_WTERMSIG :: CInt -> CInt 

foreign import ccall unsafe "__hsunix_wifstopped"
  c_WIFSTOPPED :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wstopsig"
  c_WSTOPSIG :: CInt -> CInt

foreign import ccall unsafe "__hsunix_wcoredump"
  c_WCOREDUMP :: CInt -> CInt

