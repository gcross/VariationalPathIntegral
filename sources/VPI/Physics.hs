-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.2076:@thin Physics.hs
-- @@language Haskell

module VPI.Physics where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.2078:<< Import needed modules >>
import Data.NDArray

import Foreign.Marshal.Array
import Foreign.Storable

import VPI.Updatable
import VPI.Subrangeable
-- @-node:gcross.20100106124611.2078:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100106124611.2081:Types
-- @+node:gcross.20100106124611.2082:Potential
newtype Potential = Potential { unwrapPotential :: Array1D Double }
-- @-node:gcross.20100106124611.2082:Potential
-- @+node:gcross.20100107114651.1439:TrialDerivatives
data TrialDerivatives = TrialDerivatives
    {   trialGradient :: Array2D Double
    ,   trialLaplacian :: Double
    }
-- @-node:gcross.20100107114651.1439:TrialDerivatives
-- @-node:gcross.20100106124611.2081:Types
-- @+node:gcross.20100111122429.1739:Instances
-- @+node:gcross.20100111122429.1740:Updatable Potential
instance Updatable Potential where
    update (Potential old_potential) start_slice (Potential updated_potential) =
        Potential $ update old_potential start_slice updated_potential
-- @-node:gcross.20100111122429.1740:Updatable Potential
-- @+node:gcross.20100111122429.2052:Subrangeable Potential
instance Subrangeable Potential where
    subrange start_slice end_slice = Potential . cut (Range start_slice end_slice :. ()) . unwrapPotential
-- @-node:gcross.20100111122429.2052:Subrangeable Potential
-- @-node:gcross.20100111122429.1739:Instances
-- @-others
-- @-node:gcross.20100106124611.2076:@thin Physics.hs
-- @-leo
