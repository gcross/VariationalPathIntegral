-- @+leo-ver=4-thin
-- @+node:gcross.20100111215927.1540:@thin Observable.hs
-- @@language Haskell

module VPI.Observable where

-- @<< Import needed modules >>
-- @+node:gcross.20100111215927.1541:<< Import needed modules >>
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.IORef

import VPI.Estimator
import VPI.Physics
-- @-node:gcross.20100111215927.1541:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1556:Types
-- @+node:gcross.20100111215927.1557:Observable
data Observable = Observable
    {   observableUpdater :: Configuration -> IO ()
    ,   observableSummarizer :: IO (Double,Double)
    }
-- @-node:gcross.20100111215927.1557:Observable
-- @-node:gcross.20100111215927.1556:Types
-- @+node:gcross.20100111215927.1542:Functions
-- @+node:gcross.20100111215927.1543:createObservable
createObservable :: (Configuration -> Double) -> IO Observable
createObservable observe =
    newIORef emptyEstimator
    >>=
    \estimator_ref ->
        return $
            Observable
                (evaluate . observe >=> modifyIORef estimator_ref . flip updateEstimator)
                (fmap summarizeEstimator (readIORef estimator_ref))
-- @-node:gcross.20100111215927.1543:createObservable
-- @-node:gcross.20100111215927.1542:Functions
-- @-others
-- @-node:gcross.20100111215927.1540:@thin Observable.hs
-- @-leo
