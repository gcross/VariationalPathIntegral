-- @+leo-ver=4-thin
-- @+node:gcross.20100111215927.1515:@thin Algorithm.hs
-- @@language Haskell

module VPI.Algorithm where

-- @<< Import needed modules >>
-- @+node:gcross.20100111215927.1518:<< Import needed modules >>
import Control.Monad

import VPI.Path
import VPI.Physics
import VPI.Thermalize
-- @nonl
-- @-node:gcross.20100111215927.1518:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1517:Functions
-- @+node:gcross.20100111215927.1516:runSimulation
runSimulation ::
    (Path -> IO (Int,Path)) ->
    (Path -> Potential) ->
    (Int -> Configuration -> Double) ->
    (PathSlice -> Double) ->
    (Configuration -> IO ()) ->
    Int ->
    Int ->
    Int ->
    Configuration ->
    IO Configuration
runSimulation
    generateMove
    computePotential
    computeGreensFunction
    computeTrialWeight
    updateObservables
    number_of_thermalizations_to_burn
    number_of_thermalizations_per_observation
    number_of_observations
    = runThermalization number_of_thermalizations_to_burn
      >=>
      go number_of_observations
  where
    runThermalization =
        thermalizeRepeatedly
            generateMove
            computePotential
            computeGreensFunction
            computeTrialWeight

    go 0 = return
    go n = do
        runThermalization number_of_thermalizations_per_observation
        >=>
        \thermalized_configuration ->
            updateObservables thermalized_configuration
            >>
            go (n-1) thermalized_configuration
-- @-node:gcross.20100111215927.1516:runSimulation
-- @-node:gcross.20100111215927.1517:Functions
-- @-others
-- @-node:gcross.20100111215927.1515:@thin Algorithm.hs
-- @-leo
