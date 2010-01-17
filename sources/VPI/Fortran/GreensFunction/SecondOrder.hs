-- @+leo-ver=4-thin
-- @+node:gcross.20100106124611.1996:@thin SecondOrder.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100106124611.1997:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20100106124611.1997:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100106124611.1998:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.greens_function.second_order o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.greens_function.second_order o #-}
-- @-node:gcross.20100106124611.1998:<< Link dependencies >>
-- @nl

module VPI.Fortran.GreensFunction.SecondOrder where

-- @<< Import needed modules >>
-- @+node:gcross.20100106124611.1999:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import Data.Vec ((:.)(..))

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20100106124611.1999:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100106124611.2000:initialize_weights
foreign import ccall unsafe "vpic__greens_function__second_order__initialize_weights" vpi__greens_function__second_order__initialize_weights :: 
    Int -> -- number of slices
    Ptr (Double) -> -- weights
    IO ()

initialize_weights :: Int -> Array1D Double
initialize_weights number_of_slices  =
    fst . unsafePerformIO $
    withNewNDArray (shape1 number_of_slices) $ \p_weights ->
        vpi__greens_function__second_order__initialize_weights
            number_of_slices
            p_weights
-- @-node:gcross.20100106124611.2000:initialize_weights
-- @+node:gcross.20100106124611.2015:compute_log_greens_function
foreign import ccall unsafe "vpic__greens_function__second_order__compute_log_greens_function" vpi__greens_function__second_order__compute_log_greens_function :: 
    Int -> -- number of slices
    Double -> -- slice time interval
    Ptr (Double) -> -- weights
    Ptr (Double) -> -- potential
    IO Double

compute_log_greens_function :: Double -> Array1D Double -> Array1D Double -> Double
compute_log_greens_function slice_time_interval weights potential =
    assert (shape1 number_of_slices == ndarrayShape potential)
    unsafePerformIO $
    withContiguousNDArray weights $ \p_weights ->
    withContiguousNDArray potential $ \p_potential ->
        vpi__greens_function__second_order__compute_log_greens_function
            number_of_slices
            slice_time_interval
            p_weights
            p_potential
  where
    number_of_slices :. () = ndarrayShape weights
-- @-node:gcross.20100106124611.2015:compute_log_greens_function
-- @-others
-- @-node:gcross.20100106124611.1996:@thin SecondOrder.hs
-- @-leo
