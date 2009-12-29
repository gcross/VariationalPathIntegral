-- @+leo-ver=4-thin
-- @+node:gcross.20091227115154.1353:@thin HarmonicOscillator.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091227115154.1354:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20091227115154.1354:<< Language extensions >>
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

compute_potential :: Array1D Double -> Array3D Double -> Array2D Double
compute_potential coefficients particle_positions =
    assert (shape1 number_of_dimensions == ndarrayShape coefficients) $
    fst . unsafePerformIO $
    withNDArray coefficients $ \p_coefficients ->
    withNDArray particle_positions $ \p_particle_positions ->
    withNewNDArray (shape2 number_of_slices number_of_particles) $ \p_potential ->
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
-- @-others
-- @-node:gcross.20091227115154.1353:@thin HarmonicOscillator.hs
-- @-leo
