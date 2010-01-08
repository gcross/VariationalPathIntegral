-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1440:@thin Observables.hs
-- @@language Haskell

module VPI.Observables where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1442:<< Import needed modules >>
import VPI.Fortran.Observables
import VPI.Physics
-- @nonl
-- @-node:gcross.20100107114651.1442:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100107114651.1443:computeEnergy
computeEnergy :: Double -> Double -> TrialDerivatives -> Double
computeEnergy hbar_over_2m potential trial_derivatives = compute_energy hbar_over_2m potential (trialGradient trial_derivatives) (trialLaplacian trial_derivatives)
-- @-node:gcross.20100107114651.1443:computeEnergy
-- @-others
-- @-node:gcross.20100107114651.1440:@thin Observables.hs
-- @-leo
