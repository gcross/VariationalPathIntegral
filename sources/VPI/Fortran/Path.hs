-- @+leo-ver=4-thin
-- @+node:gcross.20091217090302.1306:@thin Path.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091220132355.1793:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20091220132355.1793:<< Language extensions >>
-- @nl

module VPI.Fortran.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20091220132355.1794:<< Import needed modules >>
import Control.Arrow

import Data.NDArray
import Data.Vec (Vec3,(:.)(..))

import Foreign.Marshal.Array
import Foreign.Ptr
-- @-node:gcross.20091220132355.1794:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220132355.1792:create_initial_path
foreign import ccall unsafe "vpi__path__create_initial_path" vpi__path__create_initial_path :: 
    Int -> -- number of dimensions
    Int -> -- number of particles
    Int -> -- number of slices
    Ptr (Double) -> -- lower bounds
    Ptr (Double) -> -- upper bounds
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- particle slices
    IO ()

create_initial_path ::
    Int -> Int -> [(Double,Double)] ->
    IO (NDArray (Vec3 Int) Double,NDArray (Vec3 Int) Double)
create_initial_path number_of_slices number_of_particles bounds =
    fmap (second fst) $
    withArray lower_bounds $ \p_lower_bounds ->
    withArray upper_bounds $ \p_upper_bounds ->
    withNewNDArray (number_of_slices :. number_of_particles :. number_of_dimensions :. ()) $ \p_positions ->
    withNewNDArray (number_of_slices :. number_of_particles :. number_of_particles :. ()) $ \p_separations ->
        vpi__path__create_initial_path
            number_of_slices
            number_of_particles
            number_of_dimensions
            p_lower_bounds
            p_upper_bounds
            p_positions
            p_separations
  where
    number_of_dimensions = length bounds
    (lower_bounds,upper_bounds) = unzip bounds
-- @-node:gcross.20091220132355.1792:create_initial_path
-- @-others
-- @-node:gcross.20091217090302.1306:@thin Path.hs
-- @-leo
