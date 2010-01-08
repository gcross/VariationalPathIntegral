-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.2076:@thin Physics.hs
-- @@language Haskell

module VPI.Physics where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.2078:<< Import needed modules >>
import Data.NDArray
-- @-node:gcross.20100106124611.2078:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100106124611.2081:Types
-- @+node:gcross.20100106124611.2082:Potential
newtype Potential = Potential (Array1D Double)
-- @-node:gcross.20100106124611.2082:Potential
-- @+node:gcross.20100107114651.1439:TrialDerivatives
data TrialDerivatives = TrialDerivatives
    {   trialGradient :: Array2D Double
    ,   trialLaplacian :: Double
    }
-- @-node:gcross.20100107114651.1439:TrialDerivatives
-- @-node:gcross.20100106124611.2081:Types
-- @-others
-- @-node:gcross.20100106124611.2076:@thin Physics.hs
-- @-leo
