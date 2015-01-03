{-# LINE 1 "./Test/Framework/Runners/Core.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Core.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Core.hs" #-}
module Test.Framework.Runners.Core (
        RunTest(..), RunningTest, SomeImproving(..), FinishedTest, runTests,
        TestRunner(..), runTestTree
    ) where

import Test.Framework.Core
import Test.Framework.Improving
import Test.Framework.Options
import Test.Framework.Runners.Options
import Test.Framework.Runners.TestPattern
import Test.Framework.Runners.ThreadPool
import Test.Framework.Seed
import Test.Framework.Utilities

import Control.Concurrent.MVar
import Control.Exception (mask, finally, onException)
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Typeable


-- | A test that has been executed or is in the process of execution
data RunTest a = RunTest TestName TestTypeName a
               | RunTestGroup TestName [RunTest a]
               deriving (Show)

data SomeImproving = forall i r. TestResultlike i r => SomeImproving (i :~> r)
type RunningTest = RunTest SomeImproving

type FinishedTest = RunTest (String, Bool)

runTests :: CompleteRunnerOptions -- ^ Top-level runner options
         -> [Test]                -- ^ Tests to run
         -> IO [RunningTest]
runTests ropts tests = do
    let test_patterns = unK $ ropt_test_patterns ropts
        test_options  = unK $ ropt_test_options  ropts
    (run_tests, actions) <- runTests' $ map (runTestTree test_options test_patterns) tests
    _ <- executeOnPool (unK $ ropt_threads ropts) actions
    return run_tests

-- | 'TestRunner' class simplifies folding a 'Test'. You need to specify
-- the important semantic actions by instantiating this class, and
-- 'runTestTree' will take care of recursion and test filtering.
class TestRunner b where
    -- | How to handle a single test
    runSimpleTest :: (Testlike i r t, Typeable t) => TestOptions -> TestName -> t -> b
    -- | How to skip a test that doesn't satisfy the pattern
    skipTest :: b
    -- | How to handle an IO test (created with 'buildTestBracketed')
    runIOTest :: IO (b, IO ()) -> b
    -- | How to run a test group
    runGroup :: TestName -> [b] -> b

-- | Run the test tree using a 'TestRunner'
runTestTree
    :: TestRunner b
    => TestOptions
    -> [TestPattern]
    -- ^ skip the tests that do not match any of these patterns, unless
    -- the list is empty
    -> Test
    -> b
runTestTree initialOpts pats topTest = go initialOpts [] topTest
    where
    go opts path t = case t of
        Test name testlike ->
            if null pats || any (`testPatternMatches` (path ++ [name])) pats
                then runSimpleTest opts name testlike
                else skipTest
        TestGroup name tests ->
            let path' = path ++ [name]
            in runGroup name $ map (go opts path') tests
        PlusTestOptions extra_topts test -> go (opts `mappend` extra_topts) path test
        BuildTestBracketed build ->
            runIOTest $ onLeft (go opts path) `fmap` build

newtype StdRunner = StdRunner { run :: IO (Maybe (RunningTest, [IO ()])) }

instance TestRunner StdRunner where
    runSimpleTest topts name testlike = StdRunner $ do
        (result, action) <- runTest (completeTestOptions topts) testlike
        return (Just (RunTest name (testTypeName testlike) (SomeImproving result), [action]))

    skipTest = StdRunner $ return Nothing

    runGroup name tests = StdRunner $ do
        (results, actions) <- runTests' tests
        return $ if null results then Nothing else Just ((RunTestGroup name results), actions)

    runIOTest ioTest = StdRunner $ mask $ \restore -> ioTest >>= \(StdRunner test, cleanup) -> do
        mb_res <- restore test `onException` cleanup
        case mb_res of
            -- No sub-tests: perform the cleanup NOW
            Nothing                  -> cleanup >> return Nothing
            Just (run_test, actions) -> do
                -- Sub-tests: perform the cleanup as soon as each of them have completed
                (mvars, actions') <- liftM unzip $ forM actions $ \action -> do
                    mvar <- newEmptyMVar
                    return (mvar, action `finally` putMVar mvar ())
                -- NB: the takeMVar action MUST be last in the list because the returned actions are
                -- scheduled left-to-right, and we want all the actions we depend on to be scheduled
                -- before we wait for them to complete, or we might deadlock.
                --
                -- FIXME: this is a bit of a hack because it uses one pool thread just waiting
                -- for some other pool threads to complete! Switch to parallel-io?
                return $ Just (run_test, actions' ++ [(cleanup >> mapM_ takeMVar mvars)])

runTests' :: [StdRunner] -> IO ([RunningTest], [IO ()])
runTests' = fmap (onRight concat . unzip . catMaybes) . mapM run

completeTestOptions :: TestOptions -> CompleteTestOptions
completeTestOptions to = TestOptions {
            topt_seed = K $ topt_seed to `orElse` RandomSeed,
            topt_maximum_generated_tests = K $ topt_maximum_generated_tests to `orElse` 100,
            topt_maximum_unsuitable_generated_tests = K $ topt_maximum_unsuitable_generated_tests to `orElse` 1000,
            topt_maximum_test_size = K $ topt_maximum_test_size to `orElse` 100,
            topt_maximum_test_depth = K $ topt_maximum_test_depth to `orElse` 5,
            topt_timeout = K $ topt_timeout to `orElse` Nothing
        }
