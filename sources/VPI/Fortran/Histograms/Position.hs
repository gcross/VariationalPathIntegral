-- @+leo-ver=4-thin
-- @+node:gcross.20100109140101.1554:@thin Position.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100109140101.1555:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20100109140101.1555:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100109140101.1556:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.histograms.position o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.histograms.position o #-}
-- @-node:gcross.20100109140101.1556:<< Link dependencies >>
-- @nl

module VPI.Fortran.Histograms.Position where

-- @<< Import needed modules >>
-- @+node:gcross.20100109140101.1557:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.Int
import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import Data.NDArray.Mutable
import Data.Vec ((:.)(..),get,n1)

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20100109140101.1557:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100109140101.1558:bin_all_1d_integrated_slices
foreign import ccall unsafe "vpic__histograms__position__bin_all_1d_integrated_slices" vpi__histograms__position__bin_all_1d_integrated_slices :: 
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr Double -> -- particle positions
    Int -> -- number of bins
    Ptr Double -> -- lower bounds
    Ptr Double -> -- upper bounds
    Ptr Int32 -> -- histogram
    IO ()

bin_all_1d_integrated_slices :: Array1D Double -> Array1D Double -> MutableArray2D Int32 -> Array2D Double -> IO ()
bin_all_1d_integrated_slices lower_bounds upper_bounds histogram particle_positions =
    assert (shape1 number_of_dimensions == ndarrayShape lower_bounds) $
    assert (shape1 number_of_dimensions == ndarrayShape upper_bounds) $
    assert (shape2 number_of_dimensions number_of_bins == ndarrayShape histogram) $
    withContiguousNDArray particle_positions $ \p_particle_positions ->
    withContiguousNDArray lower_bounds $ \p_lower_bounds ->
    withContiguousNDArray upper_bounds $ \p_upper_bounds ->
    withContiguousNDArray histogram $ \p_histogram ->
        vpi__histograms__position__bin_all_1d_integrated_slices
            number_of_particles
            number_of_dimensions
            p_particle_positions
            number_of_bins
            p_lower_bounds
            p_upper_bounds
            p_histogram
  where
    number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
    number_of_bins = get n1 (ndarrayShape histogram)
-- @-node:gcross.20100109140101.1558:bin_all_1d_integrated_slices
-- @-others
-- @-node:gcross.20100109140101.1554:@thin Position.hs
-- @-leo
