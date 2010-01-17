-- @+leo-ver=4-thin
-- @+node:gcross.20100114153410.1581:@thin SecondOrder.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100114153410.1582:<< Language extensions >>
{-# LANGUAGE TypeFamilies #-}
-- @-node:gcross.20100114153410.1582:<< Language extensions >>
-- @nl

module VPI.GreensFunction.SecondOrder where

-- @<< Import needed modules >>
-- @+node:gcross.20100114153410.1583:<< Import needed modules >>
import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Indexable
import Data.Vec ((:.)(..))
import qualified Data.Vec as V

import VPI.Fortran.GreensFunction.SecondOrder
import VPI.Path
import VPI.Physics
import VPI.Spliceable
-- @-node:gcross.20100114153410.1583:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100114153410.1586:Types
-- @+node:gcross.20100114153410.1587:Weights
newtype Weights = Weights { unwrapWeights :: Array1D Double }
-- @-node:gcross.20100114153410.1587:Weights
-- @-node:gcross.20100114153410.1586:Types
-- @+node:gcross.20100114153410.1594:Instances
-- @+node:gcross.20100114153410.1597:Spliceable Weights
instance Spliceable Weights where
    type SliceResult Weights = Double
    slice index = (! i1 index) . unwrapWeights
    subrange_ start_slice end_slice = Weights . cut (Range start_slice end_slice :. ()) . unwrapWeights
    update (Weights old_weights) start_slice (Weights updated_weights) =
        Weights $ updateNDArray old_weights start_slice updated_weights
    numberOfSlices = V.head . ndarrayShape . unwrapWeights
-- @-node:gcross.20100114153410.1597:Spliceable Weights
-- @-node:gcross.20100114153410.1594:Instances
-- @+node:gcross.20100114153410.1584:Functions
-- @+node:gcross.20100114153410.1585:createWeights
createWeights :: Int -> Weights
createWeights = Weights . initialize_weights
-- @-node:gcross.20100114153410.1585:createWeights
-- @+node:gcross.20100114153410.1588:computeLogGreensFunction
computeLogGreensFunction :: Double -> Weights -> Potential -> Double
computeLogGreensFunction slice_time_interval (Weights weights) (Potential potential) =
    compute_log_greens_function slice_time_interval weights potential
-- @-node:gcross.20100114153410.1588:computeLogGreensFunction
-- @-node:gcross.20100114153410.1584:Functions
-- @-others
-- @-node:gcross.20100114153410.1581:@thin SecondOrder.hs
-- @-leo
