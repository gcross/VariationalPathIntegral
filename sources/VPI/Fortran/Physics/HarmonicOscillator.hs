-- @+leo-ver=4-thin
-- @+node:gcross.20091227115154.1353:@thin HarmonicOscillator.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091227115154.1354:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20091227115154.1354:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100105133218.1365:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.physics.harmonic_oscillator o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.physics.harmonic_oscillator o #-}
-- @-node:gcross.20100105133218.1365:<< Link dependencies >>
-- @nl

module VPI.Fortran.Physics.HarmonicOscillator where

-- @<< Import needed modules >>
-- @+node:gcross.20091227115154.1355:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.NDArray
import Data.Vec ((:.)(..))

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20091227115154.1355:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091227115154.1356:compute_potential
foreign import ccall unsafe "vpic__physics__harmonic_oscillator__compute_potential" vpi__physics__harmonic_oscillator__compute_potential :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr (Double) -> -- harmonic oscillator coefficients
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- potential
    IO ()

compute_potential :: Array1D Double -> Array3D Double -> Array1D Double
compute_potential coefficients particle_positions =
    assert (shape1 number_of_dimensions == ndarrayShape coefficients) $
    fst . unsafePerformIO $
    withContiguousNDArray coefficients $ \p_coefficients ->
    withContiguousNDArray particle_positions $ \p_particle_positions ->
    withNewNDArray (shape1 number_of_slices) $ \p_potential ->
        vpi__physics__harmonic_oscillator__compute_potential
            number_of_slices
            number_of_particles
            number_of_dimensions
            p_coefficients
            p_particle_positions
            p_potential
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20091227115154.1356:compute_potential
-- @+node:gcross.20100106124611.2029:compute_trial_weight
foreign import ccall unsafe "vpic__physics__harmonic_oscillator__compute_trial_weight" vpi__physics__harmonic_oscillator__compute_trial_weight :: 
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr (Double) -> -- harmonic oscillator coefficients
    Ptr (Double) -> -- particle positions
    IO Double

compute_trial_weight :: Array1D Double -> Array2D Double -> Double
compute_trial_weight coefficients particle_positions =
    assert (shape1 number_of_dimensions == ndarrayShape coefficients) $
    unsafePerformIO $
    withContiguousNDArray coefficients $ \p_coefficients ->
    withContiguousNDArray particle_positions $ \p_particle_positions ->
        vpi__physics__harmonic_oscillator__compute_trial_weight
            number_of_particles
            number_of_dimensions
            p_coefficients
            p_particle_positions
  where
    number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20100106124611.2029:compute_trial_weight
-- @+node:gcross.20100105133218.1367:compute_trial_derivatives
foreign import ccall unsafe "vpic__physics__harmonic_oscillator__compute_trial_derivatives" vpi__physics__harmonic_oscillator__compute_trial_derivatives :: 
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr (Double) -> -- harmonic oscillator trial coefficients
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- gradient_of_log_trial_fn
    IO Double

compute_trial_derivatives :: Array1D Double -> Array2D Double -> (Array2D Double,Double)
compute_trial_derivatives coefficients particle_positions =
    assert (shape1 number_of_dimensions == ndarrayShape coefficients) $
    unsafePerformIO $
    withContiguousNDArray coefficients $ \p_coefficients ->
    withContiguousNDArray particle_positions $ \p_particle_positions ->
    withNewNDArray (shape2 number_of_particles number_of_dimensions) $ \p_gradient_of_log_trial_fn ->
        vpi__physics__harmonic_oscillator__compute_trial_derivatives
            number_of_particles
            number_of_dimensions
            p_coefficients
            p_particle_positions
            p_gradient_of_log_trial_fn
  where
    number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20100105133218.1367:compute_trial_derivatives
-- @-others
-- @-node:gcross.20091227115154.1353:@thin HarmonicOscillator.hs
-- @-leo
