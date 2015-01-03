{-# LINE 1 "./Test/Framework/Seed.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Seed.hs" #-}
{-# LINE 1 "./Test/Framework/Seed.hs" #-}
module Test.Framework.Seed where

import Test.Framework.Utilities

import System.Random

import Data.Char


data Seed = FixedSeed Int
          | RandomSeed

instance Show Seed where
    show RandomSeed    = "random"
    show (FixedSeed n) = show n

instance Read Seed where
    readsPrec prec xs = if map toLower random_prefix == "random"
                        then [(RandomSeed, rest)]
                        else map (FixedSeed `onLeft`) (readsPrec prec xs)
      where (random_prefix, rest) = splitAt 6 xs

-- | Given a 'Seed', returns a new random number generator based on that seed and the
-- actual numeric seed that was used to build that generator, so it can be recreated.
newSeededStdGen :: Seed -> IO (StdGen, Int)
newSeededStdGen (FixedSeed seed) = return $ (mkStdGen seed, seed)
newSeededStdGen RandomSeed = newStdGenWithKnownSeed

newStdGenWithKnownSeed :: IO (StdGen, Int)
newStdGenWithKnownSeed = do
    seed <- randomIO
    return (mkStdGen seed, seed)
