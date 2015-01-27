{-# LINE 1 "Network/Sendfile.hs" #-}
# 1 "Network/Sendfile.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1




































































































# 9 "<command-line>" 2
# 1 "Network/Sendfile.hs"
{-# LANGUAGE CPP #-}

{-|
  Cross platform library for the sendfile system call.
  This library tries to call minimum system calls which
  are the bottleneck of web servers.
-}

module Network.Sendfile (
    sendfile
  , sendfileWithHeader

  , sendfileFd
  , sendfileFdWithHeader

  , FileRange(..)
  ) where

import Network.Sendfile.Types






import Network.Sendfile.Linux
