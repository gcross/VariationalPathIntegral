-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.2076:@thin Physics.hs
-- @@language Haskell

module VPI.Physics where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.2078:<< Import needed modules >>
import Data.NDArray

import Foreign.Marshal.Array
import Foreign.Storable
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
-- @+node:gcross.20100111122429.1739:Instances
-- @+node:gcross.20100111122429.1740:Updatable Potential
-- @+at
--  instance Updatable Potential where
--      update (Potential old_potential) start_slice (Potential 
--  updated_potential) =
--          unsafePerformIO $
--          withContiguousNDArray old_potential $ \p_old_potential
--          withContiguousNDArray updated_potential $ \p_updated_potential
--          withNewNDArray (ndarrayShape old_potential) $ \p_new_potential -> 
--  do
--        where
--          number_of_slices :. () = ndarrayShape old_potential
--          number_of_slices_to_update :. () = ndarrayShape old_potential
-- @-at
-- @@c
-- @-node:gcross.20100111122429.1740:Updatable Potential
-- @-node:gcross.20100111122429.1739:Instances
-- @-others
-- @-node:gcross.20100106124611.2076:@thin Physics.hs
-- @-leo
