-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1440:@thin Estimator.hs
-- @@language Haskell

module VPI.Estimator where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1442:<< Import needed modules >>
-- @-node:gcross.20100107114651.1442:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1520:Types
-- @+node:gcross.20100111215927.1534:Estimator
data Estimator = Estimator
        {   estimatorCount :: !Int
        ,   estimatorSum :: !Double
        ,   estimatorSumOfSquares :: !Double
        }
-- @-node:gcross.20100111215927.1534:Estimator
-- @-node:gcross.20100111215927.1520:Types
-- @+node:gcross.20100111215927.1522:Functions
-- @+node:gcross.20100111215927.1533:emptyEstimator
emptyEstimator = Estimator 0 0 0
-- @-node:gcross.20100111215927.1533:emptyEstimator
-- @+node:gcross.20100111215927.1538:updateEstimator
updateEstimator :: Estimator -> Double -> Estimator
updateEstimator estimator observation =
    Estimator
    {   estimatorCount = estimatorCount estimator + 1
    ,   estimatorSum = estimatorSum estimator + observation
    ,   estimatorSumOfSquares = estimatorSumOfSquares estimator + observation*observation
    }
-- @-node:gcross.20100111215927.1538:updateEstimator
-- @+node:gcross.20100111215927.1539:summarizeEstimator
summarizeEstimator :: Estimator -> (Double,Double)
summarizeEstimator estimator = (mean,variance)
  where
    count = (fromIntegral . estimatorCount) estimator
    mean = ((/count) . estimatorSum) estimator
    mean_of_squares = ((/count) . estimatorSumOfSquares) estimator
    variance = mean_of_squares - mean*mean
-- @-node:gcross.20100111215927.1539:summarizeEstimator
-- @-node:gcross.20100111215927.1522:Functions
-- @-others
-- @-node:gcross.20100107114651.1440:@thin Estimator.hs
-- @-leo
