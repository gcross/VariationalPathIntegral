-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1440:@thin Observables.hs
-- @@language Haskell

module VPI.Observables where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1442:<< Import needed modules >>
import VPI.Fortran.Observables
import VPI.Physics
-- @nonl
-- @-node:gcross.20100107114651.1442:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1520:Types
-- @+node:gcross.20100111215927.1534:Estimator
data Estimator = Estimator
        {   estimatorCount :: Int
        ,   estimatorSum :: Int
        ,   estimatorSumOfSquares :: Int
        }
-- @-node:gcross.20100111215927.1534:Estimator
-- @-node:gcross.20100111215927.1520:Types
-- @+node:gcross.20100111215927.1522:Functions
-- @+node:gcross.20100111215927.1533:emptyEstimator
emptyEstimator = Estimator 0 0 0
-- @-node:gcross.20100111215927.1533:emptyEstimator
-- @+node:gcross.20100111215927.1538:createEstimator
updateEstimator :: Estimator -> Double -> IO ()
updateEstimator observation =

createEstimator :: IO Estimator
createEstimator = liftM3
    count_ref <- newIORef (0 :: Int)
    sum_ref <- newIORef (0 :: Double)
    square_sum_ref <- newIORef (0 :: Double)
    return $ Estimator
        {   estimatorUpdater observation = do
                modifyIORef count_ref (+1)
                modifyIORef sum_ref (+observation)
                modifyIORef square_sum_ref (+observation*observation)
            estimatorSummarizer = do
                count <- fmap fromIntegral $ readIORef count_ref
                mean <- fmap (/count) $ readIORef sum_ref
                square_mean <- (/count) $ readIORef square_sum_ref
                return (mean,square_mean - mean*mean)
        }
-- @-node:gcross.20100111215927.1538:createEstimator
-- @+node:gcross.20100111215927.1519:createObservable
createObservable :: (Configuration -> Double) -> IO Observable
createObservable observe =
    createEstimator
    >>=
    \estimator ->
        Observable
        {   observableUpdater = estimatorUpdater estimator . observe
        ,   observableSummarizer = estimatorSummarizer
        }
-- @-node:gcross.20100111215927.1519:createObservable
-- @-node:gcross.20100111215927.1522:Functions
-- @-others
-- @-node:gcross.20100107114651.1440:@thin Observables.hs
-- @-leo
