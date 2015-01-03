{-# LINE 1 "./Test/Framework/Runners/TimedConsumption.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/TimedConsumption.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/TimedConsumption.hs" #-}
module Test.Framework.Runners.TimedConsumption (
        consumeListInInterval
    ) where

import Test.Framework.Utilities

import System.CPUTime


-- | Evaluates the given list for the given number of microseconds. After the time limit
-- has been reached, a list is returned consisting of the prefix of the list that was
-- successfully evaluated within the time limit.
--
-- This function does /not/ evaluate the elements of the list: it just ensures that the
-- list spine arrives in good order.
--
-- The spine of the list is evaluated on the current thread, so if spine evaluation blocks
-- this function will also block, potentially for longer than the specificed delay.
consumeListInInterval :: Int -> [a] -> IO [a]
consumeListInInterval delay list = do
    initial_time_ps <- getCPUTime
    go initial_time_ps (microsecondsToPicoseconds (fromIntegral delay)) list
  where
    go _               _        []     = return []
    go initial_time_ps delay_ps (x:xs) = do
        this_time <- getCPUTime
        if this_time - initial_time_ps < delay_ps
         then go initial_time_ps delay_ps xs >>= return . (x:)
         else return []
