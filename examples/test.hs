-- @+leo-ver=4-thin
-- @+node:gcross.20100114153410.1564:@thin test.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20100114153410.1567:<< Import needed modules >>
import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.Vec ((:.)(..))

import System.Random

import VPI.Algorithm
import qualified VPI.GreensFunction.SecondOrder as SecondOrder
import VPI.Observable
import VPI.Observable.Energy
import qualified VPI.Histogram.Position.Integrated1DSlices as Integrated1DSlices
import VPI.Path
import VPI.Path.Moves
import VPI.Physics
import qualified VPI.Physics.HarmonicOscillator as HarmonicOscillator
import VPI.Sliceable
import VPI.Subrangeable
-- @-node:gcross.20100114153410.1567:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100114153410.1572:Parameters
hbar_over_2m = 0.5

number_of_slices = 402
number_of_particles = 4
number_of_dimensions = 1

number_of_thermalizations_to_burn = 100
number_of_thermalizations_per_observation = 8
number_of_observations = 10000
-- @-node:gcross.20100114153410.1572:Parameters
-- @+node:gcross.20100114153410.1573:Values
harmonic_oscillator_coefficients = fromList (replicate number_of_dimensions 1)
greens_function_weights = SecondOrder.createWeights number_of_slices
-- @-node:gcross.20100114153410.1573:Values
-- @+node:gcross.20100114153410.1565:Functions
-- @+node:gcross.20100114153410.1566:generateMove
generateMove old_path =
    randomRIO (0,pathNumberOfParticles old_path-1)
    >>=
    flip (rigidMove 0.5) old_path
    >>=
    return . ((,) 0)
-- @-node:gcross.20100114153410.1566:generateMove
-- @+node:gcross.20100114153410.1590:computePotential
computePotential = HarmonicOscillator.computePotential harmonic_oscillator_coefficients
-- @-node:gcross.20100114153410.1590:computePotential
-- @+node:gcross.20100114153410.1589:computeLogGreensFunction
computeLogGreensFunction start_slice configuration =
    SecondOrder.computeLogGreensFunction weights potential
  where
    potential = configurationPotential configuration
    number_of_slices = numberOfSlices potential
    weights = subrange start_slice (start_slice+number_of_slices) greens_function_weights
-- @-node:gcross.20100114153410.1589:computeLogGreensFunction
-- @+node:gcross.20100114153410.1591:computeTrialWeight
computeTrialWeight = HarmonicOscillator.computeTrialWeight harmonic_oscillator_coefficients
-- @-node:gcross.20100114153410.1591:computeTrialWeight
-- @+node:gcross.20100114153410.1593:computeTrialDerivatives
computeTrialDerivatives = HarmonicOscillator.computeTrialDerivatives harmonic_oscillator_coefficients
-- @-node:gcross.20100114153410.1593:computeTrialDerivatives
-- @-node:gcross.20100114153410.1565:Functions
-- @+node:gcross.20100114153410.1592:main
main = do
    Observable updateEnergyObservable summarizeEnergyObservable <- createEnergyObservable hbar_over_2m computeTrialDerivatives
    histogram <- Integrated1DSlices.createEmptyHistogram 50 [(-2,2)]
    let updateObservables configuration =
            mapM_ ($ configuration)
                [updateEnergyObservable
                ,Integrated1DSlices.updateHistogramFromConfiguration histogram 0
                ]
    createInitialPath number_of_slices number_of_particles (replicate number_of_dimensions (-1,1))
        >>=
        (return . makeConfigurationFromPath computePotential)
        >>=
        runSimulation
            generateMove
            computePotential
            computeLogGreensFunction
            computeTrialWeight
            updateObservables
            number_of_thermalizations_to_burn
            number_of_thermalizations_per_observation
            number_of_observations
    (energy,energy_error) <- summarizeEnergyObservable
    putStrLn $ "Energy = " ++ show energy ++ " +/- " ++ show energy_error
    Integrated1DSlices.writeHistogramToFiles ["data"] histogram
-- @-node:gcross.20100114153410.1592:main
-- @-others
-- @-node:gcross.20100114153410.1564:@thin test.hs
-- @-leo
