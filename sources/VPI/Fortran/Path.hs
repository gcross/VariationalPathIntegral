-- @+leo-ver=4-thin
-- @+node:gcross.20091217090302.1306:@thin Path.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091220132355.1793:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- @-node:gcross.20091220132355.1793:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100105133218.1361:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.path o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.path o #-}
-- @-node:gcross.20100105133218.1361:<< Link dependencies >>
-- @nl

module VPI.Fortran.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20091220132355.1794:<< Import needed modules >>
import Control.Arrow
import Control.Exception

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import Data.Vec ((:.)(..),get,n0)

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20091220132355.1794:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220132355.1792:compute_separations
foreign import ccall unsafe "vpic__path__compute_separations" vpi__path__compute_separations :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- particle separations
    IO ()

compute_separations :: Array3D Double -> Array3D Double
compute_separations particle_positions =
    fst . unsafePerformIO $
    withContiguousNDArray particle_positions $ \p_positions ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_particles) $ \p_separations ->
        vpi__path__compute_separations
            number_of_slices
            number_of_particles
            number_of_dimensions
            p_positions
            p_separations
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20091220132355.1792:compute_separations
-- @+node:gcross.20100116114537.1612:update_separations_for_particle
foreign import ccall unsafe "vpic__path__update_separations_for_particle" vpi__path__update_separations_for_particle :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Int -> -- particle number that changed
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- old particle separations
    Ptr (Double) -> -- new particle separations
    IO ()

update_separations_for_particle :: Int -> Array3D Double -> Array3D Double -> Array3D Double
update_separations_for_particle particle_number particle_positions old_particle_separations =
    assert (shape3 number_of_slices number_of_particles number_of_particles == ndarrayShape old_particle_separations) $
    fst . unsafePerformIO $
    withContiguousNDArray particle_positions $ \p_positions ->
    withContiguousNDArray old_particle_separations $ \p_old_separations ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_particles) $ \p_new_separations ->
        vpi__path__update_separations_for_particle
            number_of_slices
            number_of_particles
            number_of_dimensions
            particle_number
            p_positions
            p_old_separations
            p_new_separations
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20100116114537.1612:update_separations_for_particle
-- @+node:gcross.20091226065853.1634:create_initial_path
foreign import ccall unsafe "vpic__path__create_initial_path" vpi__path__create_initial_path :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Ptr (Double) -> -- lower bounds
    Ptr (Double) -> -- upper bounds
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- particle slices
    IO ()

create_initial_path ::
    Int -> Int -> [(Double,Double)] ->
    IO (Array3D Double,Array3D Double)
create_initial_path number_of_slices number_of_particles bounds =
    fmap (second fst) $
    withArray lower_bounds $ \p_lower_bounds ->
    withArray upper_bounds $ \p_upper_bounds ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) $ \p_positions ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_particles) $ \p_separations ->
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
-- @-node:gcross.20091226065853.1634:create_initial_path
-- @-others
-- @-node:gcross.20091217090302.1306:@thin Path.hs
-- @-leo
