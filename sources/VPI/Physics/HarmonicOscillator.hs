-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.2073:@thin HarmonicOscillator.hs
-- @@language Haskell

module VPI.Physics.HarmonicOscillator where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.2074:<< Import needed modules >>
import Data.NDArray

import VPI.Fortran.Physics.HarmonicOscillator
import VPI.Path
import VPI.Physics
-- @-node:gcross.20100106124611.2074:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100107114651.1428:Types
-- @+node:gcross.20100107114651.1430:Coefficients
type Coefficients = Array1D Double
-- @-node:gcross.20100107114651.1430:Coefficients
-- @-node:gcross.20100107114651.1428:Types
-- @+node:gcross.20100107114651.1429:Functions
-- @+node:gcross.20100106124611.2075:computePotential
computePotential :: Coefficients -> Path -> Potential
computePotential coefficients = Potential . compute_potential coefficients . pathParticlePositions
-- @-node:gcross.20100106124611.2075:computePotential
-- @+node:gcross.20100107114651.1432:computeTrialWeight
computeTrialWeight :: Coefficients -> PathSlice -> Double
computeTrialWeight coefficients = compute_trial_weight coefficients . pathSliceParticlePositions
-- @-node:gcross.20100107114651.1432:computeTrialWeight
-- @+node:gcross.20100107114651.1438:computeTrialDerivatives
computeTrialDerivatives :: Coefficients -> PathSlice -> TrialDerivatives
computeTrialDerivatives coefficients = uncurry TrialDerivatives . compute_trial_derivatives coefficients . pathSliceParticlePositions
-- @-node:gcross.20100107114651.1438:computeTrialDerivatives
-- @-node:gcross.20100107114651.1429:Functions
-- @-others
-- @-node:gcross.20100106124611.2073:@thin HarmonicOscillator.hs
-- @-leo
