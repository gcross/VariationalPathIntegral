-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1450:@thin Thermalize.hs
-- @@language Haskell

module VPI.Thermalize where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1452:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import System.Random

import VPI.Path
import VPI.Subrangeable
import VPI.Updatable
-- @-node:gcross.20100107114651.1452:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111122429.2009:Types
-- @+node:gcross.20100111122429.2011:Configuration
data Configuration potential = Configuration
        {   configurationPath :: !Path
        ,   configurationPotential :: !potential
        }
-- @-node:gcross.20100111122429.2011:Configuration
-- @-node:gcross.20100111122429.2009:Types
-- @+node:gcross.20100111122429.2012:Instances
-- @+node:gcross.20100111122429.2054:Subrangeable (Configuration potential)
instance Subrangeable potential => Subrangeable (Configuration potential) where
    subrange start_slice end_slice =
        liftA2 Configuration
            (subrange start_slice end_slice . configurationPath)
            (subrange start_slice end_slice . configurationPotential)
-- @-node:gcross.20100111122429.2054:Subrangeable (Configuration potential)
-- @+node:gcross.20100111122429.2013:Updatable (Configuration potential)
instance Updatable potential => Updatable (Configuration potential) where
    update (Configuration old_path old_potential) start_slice (Configuration updated_path updated_potential) =
        Configuration
            (update old_path start_slice updated_path)
            (update old_potential start_slice updated_potential)
-- @-node:gcross.20100111122429.2013:Updatable (Configuration potential)
-- @-node:gcross.20100111122429.2012:Instances
-- @+node:gcross.20100111122429.2010:Functions
-- @+node:gcross.20100107114651.1451:decideWhetherToAcceptChange
decideWhetherToAcceptChange :: Double -> Double -> IO Bool
decideWhetherToAcceptChange old_weight new_weight = fmap (< exp (new_weight-old_weight)) randomIO
-- @-node:gcross.20100107114651.1451:decideWhetherToAcceptChange
-- @+node:gcross.20100111122429.2008:thermalize
thermalize ::
    (Updatable potential, Subrangeable potential) =>
    (Path -> IO (Int,Path)) ->
    (Path -> potential) ->
    (Int -> potential -> Double) ->
    (PathSlice -> Double) ->
    Configuration potential ->
    IO (Configuration potential)
thermalize
    generateMove
    computePotential
    computeGreensFunction
    computeTrialWeight
    old_configuration@(Configuration path potential)
   = do (start_slice,proposed_path) <- generateMove path
        let proposed_potential = computePotential proposed_path
            end_slice = start_slice + pathLength proposed_path
            current_path = subrange start_slice end_slice path
            current_potential = subrange start_slice end_slice potential

            computePathWeight = liftA2 (+) computeFirstSliceWeight computeLastSliceWeight
              where
                computeFirstSliceWeight =
                    if start_slice == 0
                        then computeTrialWeight . firstPathSlice
                        else const 0
                computeLastSliceWeight =
                    if end_slice == pathLength path
                        then computeTrialWeight . lastPathSlice
                        else const 0

            computePotentialWeight = computeGreensFunction start_slice

        accept <- decideWhetherToAcceptChange
                    (computePathWeight current_path + computePotentialWeight current_potential)
                    (computePathWeight proposed_path + computePotentialWeight proposed_potential)
        return $
            if accept
                then Configuration (update path start_slice proposed_path)
                                   (update potential start_slice proposed_potential)
                else old_configuration
-- @-node:gcross.20100111122429.2008:thermalize
-- @+node:gcross.20100111122429.2055:thermalizeRepeatedly
thermalizeRepeatedly ::
    (Updatable potential, Subrangeable potential) =>
    (Path -> IO (Int,Path)) ->
    (Path -> potential) ->
    (Int -> potential -> Double) ->
    (PathSlice -> Double) ->
    Int ->
    Configuration potential ->
    IO (Configuration potential)
thermalizeRepeatedly _ _ _ _ 0 = return

thermalizeRepeatedly
    generateMove
    computePotential
    computeGreensFunction
    computeTrialWeight
    number_of_times
    = thermalize
        generateMove
        computePotential
        computeGreensFunction
        computeTrialWeight
      >=>
      thermalizeRepeatedly
        generateMove
        computePotential
        computeGreensFunction
        computeTrialWeight
        (number_of_times-1)
-- @-node:gcross.20100111122429.2055:thermalizeRepeatedly
-- @-node:gcross.20100111122429.2010:Functions
-- @-others
-- @-node:gcross.20100107114651.1450:@thin Thermalize.hs
-- @-leo
