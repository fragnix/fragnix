{-# LINE 1 "./Test/Framework/Improving.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Improving.hs" #-}
{-# LINE 1 "./Test/Framework/Improving.hs" #-}
module Test.Framework.Improving (
        (:~>)(..), bimapImproving, improvingLast, consumeImproving,
        ImprovingIO, yieldImprovement, runImprovingIO, tunnelImprovingIO, liftIO,
        timeoutImprovingIO, maybeTimeoutImprovingIO
    ) where

import Control.Concurrent
import Control.Monad

import System.Timeout


data i :~> f = Finished f
             | Improving i (i :~> f)

instance Functor ((:~>) i) where
    fmap f (Finished x)    = Finished (f x)
    fmap f (Improving x i) = Improving x (fmap f i)

bimapImproving :: (a -> c) -> (b -> d) -> (a :~> b) -> (c :~> d)
bimapImproving _ g (Finished b)            = Finished (g b)
bimapImproving f g (Improving a improving) = Improving (f a) (bimapImproving f g improving)

improvingLast :: (a :~> b) -> b
improvingLast (Finished r)       = r
improvingLast (Improving _ rest) = improvingLast rest

consumeImproving :: (a :~> b) -> [(a :~> b)]
consumeImproving improving@(Finished _)       = [improving]
consumeImproving improving@(Improving _ rest) = improving : consumeImproving rest


newtype ImprovingIO i f a = IIO { unIIO :: Chan (Either i f) -> IO a }

instance Functor (ImprovingIO i f) where
    fmap = liftM

instance Monad (ImprovingIO i f) where
    return x = IIO (const $ return x)
    ma >>= f = IIO $ \chan -> do
                    a <- unIIO ma chan
                    unIIO (f a) chan

yieldImprovement :: i -> ImprovingIO i f ()
yieldImprovement improvement = IIO $ \chan -> do
    -- Whenever we yield an improvement, take the opportunity to yield the thread as well.
    -- The idea here is to introduce frequent yields in users so that if e.g. they get killed
    -- by the timeout code then they know about it reasonably promptly.
    yield
    writeChan chan (Left improvement)

-- NB: could have a more general type but it would be impredicative
tunnelImprovingIO :: ImprovingIO i f (ImprovingIO i f a -> IO a)
tunnelImprovingIO = IIO $ \chan -> return $ \iio -> unIIO iio chan

runImprovingIO :: ImprovingIO i f f -> IO (i :~> f, IO ())
runImprovingIO iio = do
    chan <- newChan
    let action = do
        result <- unIIO iio chan
        writeChan chan (Right result)
    improving_value <- getChanContents chan
    return (reifyListToImproving improving_value, action)

reifyListToImproving :: [Either i f] -> (i :~> f)
reifyListToImproving (Left improvement:rest) = Improving improvement (reifyListToImproving rest)
reifyListToImproving (Right final:_)         = Finished final
reifyListToImproving []                      = error "reifyListToImproving: list finished before a final value arrived"

liftIO :: IO a -> ImprovingIO i f a
liftIO io = IIO $ const io

-- | Given a number of microseconds and an improving IO action, run that improving IO action only
-- for at most the given period before giving up. See also 'System.Timeout.timeout'.
timeoutImprovingIO :: Int -> ImprovingIO i f a -> ImprovingIO i f (Maybe a)
timeoutImprovingIO microseconds iio = IIO $ \chan -> timeout microseconds $ unIIO iio chan

-- | As 'timeoutImprovingIO', but don't bother applying a timeout to the action if @Nothing@ is given
-- as the number of microseconds to apply the time out for.
maybeTimeoutImprovingIO :: Maybe Int -> ImprovingIO i f a -> ImprovingIO i f (Maybe a)
maybeTimeoutImprovingIO Nothing             = fmap Just
maybeTimeoutImprovingIO (Just microseconds) = timeoutImprovingIO microseconds
