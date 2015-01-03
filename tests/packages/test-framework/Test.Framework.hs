{-# LINE 1 "./Test/Framework.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework.hs" #-}
{-# LINE 1 "./Test/Framework.hs" #-}
-- | A generic test framework for all types of Haskell test.
--
-- For an example of how to use test-framework, please see
-- <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
module Test.Framework (
        module Test.Framework.Core,
        module Test.Framework.Options,
        module Test.Framework.Runners.Console,
        module Test.Framework.Runners.Options,
        module Test.Framework.Seed
    ) where

import Test.Framework.Core (Test, TestName, testGroup, plusTestOptions, buildTest, buildTestBracketed, mutuallyExclusive)
import Test.Framework.Options
import Test.Framework.Runners.Console
import Test.Framework.Runners.Options
import Test.Framework.Seed
