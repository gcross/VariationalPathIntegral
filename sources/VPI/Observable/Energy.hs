-- @+leo-ver=4-thin
-- @+node:gcross.20100111215927.1524:@thin Energy.hs
-- @@language Haskell

module VPI.Observable.Energy where

-- @<< Import needed modules >>
-- @+node:gcross.20100111215927.1525:<< Import needed modules >>
import Control.Applicative

import Data.NDArray
import Data.NDArray.Indexable

import VPI.Fortran.Observables
import VPI.Observable
import VPI.Path
import VPI.Physics
import VPI.Spliceable
-- @-node:gcross.20100111215927.1525:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1526:Functions
-- @+node:gcross.20100111215927.1528:computeEnergy
computeEnergy :: Double -> Double -> TrialDerivatives -> Double
computeEnergy hbar_over_2m potential trial_derivatives =
    compute_energy hbar_over_2m potential (trialGradient trial_derivatives) (trialLaplacian trial_derivatives)
-- @-node:gcross.20100111215927.1528:computeEnergy
-- @+node:gcross.20100111215927.1562:computeEnergyAtSlice
computeEnergyAtSlice :: Double -> (PathSlice -> TrialDerivatives) -> ConfigurationSlice -> Double
computeEnergyAtSlice hbar_over_2m computeTrialDerivatives =
    liftA2 (computeEnergy hbar_over_2m)
        configurationSlicePotential
        (computeTrialDerivatives . configurationSlicePath)
-- @-node:gcross.20100111215927.1562:computeEnergyAtSlice
-- @+node:gcross.20100111215927.1529:createEnergyObservable
createEnergyObservable :: Double -> (PathSlice -> TrialDerivatives) -> IO Observable
createEnergyObservable hbar_over_2m computeTrialDerivatives =
    createObservable $
        \configuration ->
          ( computeThisEnergyAtSlice (firstSlice configuration)
          + computeThisEnergyAtSlice (lastSlice configuration)
          ) / 2
  where
    computeThisEnergyAtSlice = computeEnergyAtSlice hbar_over_2m computeTrialDerivatives
-- @-node:gcross.20100111215927.1529:createEnergyObservable
-- @-node:gcross.20100111215927.1526:Functions
-- @-others
-- @-node:gcross.20100111215927.1524:@thin Energy.hs
-- @-leo
