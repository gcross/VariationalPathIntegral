-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.2076:@thin Physics.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100111215927.1843:<< Language extensions >>
{-# LANGUAGE TypeFamilies #-}
-- @-node:gcross.20100111215927.1843:<< Language extensions >>
-- @nl

module VPI.Physics where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.2078:<< Import needed modules >>
import Control.Applicative

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Indexable
import qualified Data.Vec as V
import Data.Vec ((:.)(..))

import Foreign.Marshal.Array
import Foreign.Storable

import VPI.Path
import VPI.Sliceable
import VPI.Subrangeable
import VPI.Updatable
-- @-node:gcross.20100106124611.2078:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100106124611.2081:Types
-- @+node:gcross.20100111215927.1555:Configuration
data Configuration = Configuration
        {   configurationPath :: Path
        ,   configurationPotential :: Potential
        }
-- @-node:gcross.20100111215927.1555:Configuration
-- @+node:gcross.20100111215927.1564:ConfigurationSlice
data ConfigurationSlice = ConfigurationSlice
        {   configurationSliceNumber :: Int
        ,   configurationSlicePath :: PathSlice
        ,   configurationSlicePotential :: Double
        }
-- @-node:gcross.20100111215927.1564:ConfigurationSlice
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
-- @+node:gcross.20100111215927.1584:Sliceable
-- @+node:gcross.20100111215927.1585:Configuration
instance Sliceable Configuration where
    type SliceResult Configuration = ConfigurationSlice
    slice index =
        liftA2 (ConfigurationSlice index)
            (slice index . configurationPath)
            (slice index . configurationPotential)
    numberOfSlices = numberOfSlices . configurationPath
-- @-node:gcross.20100111215927.1585:Configuration
-- @+node:gcross.20100111215927.1587:Potential
instance Sliceable Potential where
    type SliceResult Potential = Double
    slice index = (! i1 index) . unwrapPotential
    numberOfSlices = V.head . ndarrayShape . unwrapPotential
-- @nonl
-- @-node:gcross.20100111215927.1587:Potential
-- @-node:gcross.20100111215927.1584:Sliceable
-- @+node:gcross.20100111215927.1551:Subrangeable
-- @+node:gcross.20100111215927.1553:Configuration
instance Subrangeable Configuration where
    subrange start_slice end_slice =
        liftA2 Configuration
            (subrange start_slice end_slice . configurationPath)
            (subrange start_slice end_slice . configurationPotential)
-- @-node:gcross.20100111215927.1553:Configuration
-- @+node:gcross.20100111122429.2052:Potential
instance Subrangeable Potential where
    subrange start_slice end_slice = Potential . cut (Range start_slice end_slice :. ()) . unwrapPotential
-- @-node:gcross.20100111122429.2052:Potential
-- @-node:gcross.20100111215927.1551:Subrangeable
-- @+node:gcross.20100111215927.1548:Updatable
-- @+node:gcross.20100111122429.1740:Potential
instance Updatable Potential where
    update (Potential old_potential) start_slice (Potential updated_potential) =
        Potential $ update old_potential start_slice updated_potential
-- @-node:gcross.20100111122429.1740:Potential
-- @+node:gcross.20100111215927.1550:Configuration
instance Updatable Configuration where
    update (Configuration old_path old_potential) start_slice (Configuration updated_path updated_potential) =
        Configuration
            (update old_path start_slice updated_path)
            (update old_potential start_slice updated_potential)
-- @-node:gcross.20100111215927.1550:Configuration
-- @-node:gcross.20100111215927.1548:Updatable
-- @-node:gcross.20100111122429.1739:Instances
-- @+node:gcross.20100111215927.1558:Functions
-- @+node:gcross.20100111215927.1559:makeConfigurationFromPath
makeConfigurationFromPath :: (Path -> Potential) -> Path -> Configuration
makeConfigurationFromPath computePotential path = Configuration path (computePotential path)
-- @-node:gcross.20100111215927.1559:makeConfigurationFromPath
-- @-node:gcross.20100111215927.1558:Functions
-- @-others
-- @-node:gcross.20100106124611.2076:@thin Physics.hs
-- @-leo
