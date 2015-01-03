{-# LINE 1 "./Test/Framework/Core.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Core.hs" #-}
{-# LINE 1 "./Test/Framework/Core.hs" #-}
{-# LANGUAGE UndecidableInstances, DeriveDataTypeable #-}
module Test.Framework.Core where

import Test.Framework.Improving
import Test.Framework.Options

import Control.Arrow (first, second)
import Control.Concurrent.MVar
import Data.Typeable


-- | Something like the result of a test: works in concert with 'Testlike'.
-- The type parameters are the type that is used for progress reports and the
-- type of the final output of the test respectively.
class (Show i, Show r) => TestResultlike i r | r -> i where
    testSucceeded :: r -> Bool

-- | Something test-like in its behaviour. The type parameters are the type that
-- is used for progress reports, the type of the final output of the test and the
-- data type encapsulating the whole potential to do a test respectively.
class TestResultlike i r => Testlike i r t | t -> i r, r -> i where
    runTest :: CompleteTestOptions -> t -> IO (i :~> r, IO ())
    testTypeName :: t -> TestTypeName


-- | Test names or descriptions. These are shown to the user
type TestName = String

-- | The name of a type of test, such as "Properties" or "Test Cases". Tests of
-- types of the same names will be grouped together in the test run summary.
type TestTypeName = String

-- | Main test data type: builds up a list of tests to be run. Users should use the
-- utility functions in e.g. the test-framework-hunit and test-framework-quickcheck
-- packages to create instances of 'Test', and then build them up into testsuites
-- by using 'testGroup' and lists.
--
-- For an example of how to use test-framework, please see
-- <http://github.com/batterseapower/test-framework/raw/master/example/Test/Framework/Example.lhs>
data Test = forall i r t.
            (Testlike i r t, Typeable t) => Test TestName t -- ^ A single test of some particular type
          | TestGroup TestName [Test]                       -- ^ Assemble a number of tests into a cohesive group
          | PlusTestOptions TestOptions Test                -- ^ Add some options to child tests
          | BuildTestBracketed (IO (Test, IO ()))           -- ^ Convenience for creating tests from an 'IO' action, with cleanup

-- | Assemble a number of tests into a cohesive group
testGroup :: TestName -> [Test] -> Test
testGroup = TestGroup

-- | Add some options to child tests
plusTestOptions :: TestOptions -> Test -> Test
plusTestOptions = PlusTestOptions

-- | Convenience for creating tests from an 'IO' action
buildTest :: IO Test -> Test
buildTest mx = BuildTestBracketed (fmap (flip (,) (return ())) mx)

-- | Convenience for creating tests from an 'IO' action, with a cleanup handler for when tests are finished
buildTestBracketed :: IO (Test, IO ()) -> Test
buildTestBracketed = BuildTestBracketed


data MutuallyExcluded t = ME (MVar ()) t
    deriving Typeable

-- This requires UndecidableInstances, but I think it can't be made inconsistent?
instance Testlike i r t => Testlike i r (MutuallyExcluded t) where
    runTest cto (ME mvar x) = fmap (second (\act -> withMVar mvar $ \() -> act)) $ runTest cto x
    testTypeName ~(ME _ x) = testTypeName x

-- | Mark all tests in this portion of the tree as mutually exclusive, so only one runs at a time
{-# NOINLINE mutuallyExclusive #-}
mutuallyExclusive :: Test -> Test
mutuallyExclusive init_t = buildTest $ do
    mvar <- newMVar ()
    let go (Test tn t)                = Test tn (ME mvar t)
        go (TestGroup tn ts)          = TestGroup tn (map go ts)
        go (PlusTestOptions to t)     = PlusTestOptions to (go t)
        go (BuildTestBracketed build) = BuildTestBracketed (fmap (first go) build)
    return (go init_t)
