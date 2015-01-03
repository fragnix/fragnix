{-# LINE 1 "./Test/Framework/Runners/Options.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Options.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Options.hs" #-}
module Test.Framework.Runners.Options (
        module Test.Framework.Runners.Options,
        TestPattern
    ) where

import Test.Framework.Options
import Test.Framework.Utilities
import Test.Framework.Runners.TestPattern

import Data.Monoid

data ColorMode = ColorAuto | ColorNever | ColorAlways

type RunnerOptions = RunnerOptions' Maybe
type CompleteRunnerOptions = RunnerOptions' K
data RunnerOptions' f = RunnerOptions {
        ropt_threads :: f Int,
        ropt_test_options :: f TestOptions,
        ropt_test_patterns :: f [TestPattern],
        ropt_xml_output :: f (Maybe FilePath),
        ropt_xml_nested :: f Bool,
        ropt_color_mode :: f ColorMode,
        ropt_hide_successes :: f Bool,
        ropt_list_only  :: f Bool
    }

instance Monoid (RunnerOptions' Maybe) where
    mempty = RunnerOptions {
            ropt_threads = Nothing,
            ropt_test_options = Nothing,
            ropt_test_patterns = Nothing,
            ropt_xml_output = Nothing,
            ropt_xml_nested = Nothing,
            ropt_color_mode = Nothing,
            ropt_hide_successes = Nothing,
            ropt_list_only      = Nothing
        }

    mappend ro1 ro2 = RunnerOptions {
            ropt_threads = getLast (mappendBy (Last . ropt_threads) ro1 ro2),
            ropt_test_options = mappendBy ropt_test_options ro1 ro2,
            ropt_test_patterns = mappendBy ropt_test_patterns ro1 ro2,
            ropt_xml_output = mappendBy ropt_xml_output ro1 ro2,
            ropt_xml_nested = getLast (mappendBy (Last . ropt_xml_nested) ro1 ro2),
            ropt_color_mode = getLast (mappendBy (Last . ropt_color_mode) ro1 ro2),
            ropt_hide_successes = getLast (mappendBy (Last . ropt_hide_successes) ro1 ro2),
            ropt_list_only      = getLast (mappendBy (Last . ropt_list_only)      ro1 ro2)
        }
