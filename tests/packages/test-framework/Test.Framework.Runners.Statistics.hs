{-# LINE 1 "./Test/Framework/Runners/Statistics.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Statistics.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Statistics.hs" #-}
module Test.Framework.Runners.Statistics (
        TestCount, testCountTestTypes, testCountForType, adjustTestCount, testCountTotal,
        TestStatistics(..), ts_pending_tests, ts_no_failures,
        initialTestStatistics, updateTestStatistics,
        totalRunTestsList, gatherStatistics
  ) where

import Test.Framework.Core (TestTypeName)
import Test.Framework.Runners.Core

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid


-- | Records a count of the various kinds of test that have been run
newtype TestCount = TestCount { unTestCount :: Map TestTypeName Int }

testCountTestTypes :: TestCount -> [TestTypeName]
testCountTestTypes = Map.keys . unTestCount

testCountForType :: String -> TestCount -> Int
testCountForType test_type = Map.findWithDefault 0 test_type . unTestCount

adjustTestCount :: String -> Int -> TestCount -> TestCount
adjustTestCount test_type amount = TestCount . Map.insertWith (+) test_type amount . unTestCount


-- | The number of tests of all kinds recorded in the given 'TestCount'
testCountTotal :: TestCount -> Int
testCountTotal = sum . Map.elems . unTestCount

instance Monoid TestCount where
    mempty = TestCount $ Map.empty
    mappend (TestCount tcm1) (TestCount tcm2) = TestCount $ Map.unionWith (+) tcm1 tcm2

minusTestCount :: TestCount -> TestCount -> TestCount
minusTestCount (TestCount tcm1) (TestCount tcm2) = TestCount $ Map.unionWith (-) tcm1 tcm2


-- | Records information about the run of a number of tests, such
-- as how many tests have been run, how many are pending and how
-- many have passed or failed.
data TestStatistics = TestStatistics {
        ts_total_tests :: TestCount,
        ts_run_tests :: TestCount,
        ts_passed_tests :: TestCount,
        ts_failed_tests :: TestCount
    }

instance Monoid TestStatistics where
    mempty = TestStatistics mempty mempty mempty mempty
    mappend (TestStatistics tot1 run1 pas1 fai1) (TestStatistics tot2 run2 pas2 fai2) = TestStatistics (tot1 `mappend` tot2) (run1 `mappend` run2) (pas1 `mappend` pas2) (fai1 `mappend` fai2)

ts_pending_tests :: TestStatistics -> TestCount
ts_pending_tests ts = ts_total_tests ts `minusTestCount` ts_run_tests ts

ts_no_failures :: TestStatistics -> Bool
ts_no_failures ts = testCountTotal (ts_failed_tests ts) <= 0

-- | Create some test statistics that simply records the total number of
-- tests to be run, ready to be updated by the actual test runs.
initialTestStatistics :: TestCount -> TestStatistics
initialTestStatistics total_tests = TestStatistics {
        ts_total_tests = total_tests,
        ts_run_tests = mempty,
        ts_passed_tests = mempty,
        ts_failed_tests = mempty
    }

updateTestStatistics :: (Int -> TestCount) -> Bool -> TestStatistics -> TestStatistics
updateTestStatistics count_constructor test_suceeded test_statistics = test_statistics {
        ts_run_tests    = ts_run_tests test_statistics    `mappend` (count_constructor 1),
        ts_failed_tests = ts_failed_tests test_statistics `mappend` (count_constructor (if test_suceeded then 0 else 1)),
        ts_passed_tests = ts_passed_tests test_statistics `mappend` (count_constructor (if test_suceeded then 1 else 0))
    }


totalRunTests :: RunTest a -> TestCount
totalRunTests (RunTest _ test_type _) = adjustTestCount test_type 1 mempty
totalRunTests (RunTestGroup _ tests)  = totalRunTestsList tests

totalRunTestsList :: [RunTest a] -> TestCount
totalRunTestsList = mconcat . map totalRunTests

gatherStatistics :: [FinishedTest] -> TestStatistics
gatherStatistics = mconcat . map f
  where
    f (RunTest _ test_type (_, success)) = singleTestStatistics test_type success
    f (RunTestGroup _ tests)             = gatherStatistics tests

    singleTestStatistics :: String -> Bool -> TestStatistics
    singleTestStatistics test_type success = TestStatistics {
            ts_total_tests = one,
            ts_run_tests = one,
            ts_passed_tests = if success then one else mempty,
            ts_failed_tests = if success then mempty else one
        }
      where one = adjustTestCount test_type 1 mempty
