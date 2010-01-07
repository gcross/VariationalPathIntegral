-- @+leo-ver=4-thin
-- @+node:gcross.20100105133218.1381:@thin Observables.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100105133218.1382:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20100105133218.1382:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100105133218.1383:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.observables o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.observables o #-}
-- @-node:gcross.20100105133218.1383:<< Link dependencies >>
-- @nl

module VPI.Fortran.Observables where

-- @<< Import needed modules >>
-- @+node:gcross.20100105133218.1384:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.NDArray
import Data.Vec ((:.)(..))

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20100105133218.1384:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100105133218.1385:compute_energy
foreign import ccall unsafe "vpic__observables__compute_energy" vpi__observables__compute_energy :: 
    Int -> -- number of particles
    Int -> -- number of dimensions
    Double -> -- hbar/2m
    Double -> -- potential
    Ptr (Double) -> -- gradient of log trial fn
    Double -> -- laplacian_of_log_trial_fn
    IO Double

compute_energy :: Double -> Double -> Array2D Double -> Double -> Double
compute_energy hbar_over_2m potential gradient_of_log_trial_fn laplacian_of_log_trial_fn =
    unsafePerformIO $
    withContiguousNDArray gradient_of_log_trial_fn $ \p_gradient_of_log_trial_fn ->
        vpi__observables__compute_energy
            number_of_particles
            number_of_dimensions
            hbar_over_2m
            potential
            p_gradient_of_log_trial_fn
            laplacian_of_log_trial_fn
  where
    number_of_particles :. number_of_dimensions :. () = ndarrayShape gradient_of_log_trial_fn
-- @-node:gcross.20100105133218.1385:compute_energy
-- @-others
-- @-node:gcross.20100105133218.1381:@thin Observables.hs
-- @-leo
