{-# LINE 1 "./Test/Framework/Providers/API.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Providers/API.hs" #-}
{-# LINE 1 "./Test/Framework/Providers/API.hs" #-}
-- | This module exports everything that you need to be able to create your own framework test provider.
-- To create a provider you need to:
--
-- * Create an instance of the 'Testlike' class
--
-- * Create an instance of the 'TestResultlike' class
--
-- * Expose a function that lets people construct 'Test' values using your new instances
module Test.Framework.Providers.API (
        module Test.Framework.Core,
        module Test.Framework.Improving,
        module Test.Framework.Options,
        module Test.Framework.Seed,
        module Test.Framework.Utilities
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Seed
import Test.Framework.Utilities
