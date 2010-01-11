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
import Data.Vec ((:.)(..),Vec2,Vec3)

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
    Ptr (Double) -> -- particle slices
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
-- @+node:gcross.20100111122429.1492:update_path
foreign import ccall unsafe "vpic__path__update_path" vpi__path__update_path :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Int -> -- start slice of update
    Int -> -- end slice of update
    Ptr Double -> -- old particle positions
    Ptr Double -> -- old particle separations
    Ptr Double -> -- updated particle positions
    Ptr Double -> -- updated particle separations
    Ptr Double -> -- new particle positions
    Ptr Double -> -- new particle separations
    IO ()

update_path :: Array3D Double -> Array3D Double -> Int -> Array3D Double -> Array3D Double -> (Array3D Double,Array3D Double)
update_path old_particle_positions old_particle_separations update_start_slice updated_particle_positions updated_particle_separations =
    assert (shape3 number_of_slices number_of_particles number_of_particles == ndarrayShape old_particle_separations) $
    assert (shape3 number_of_slices_to_update number_of_particles number_of_particles == ndarrayShape updated_particle_separations) $
    assert (update_end_slice <= number_of_slices) $
    second fst . unsafePerformIO $
    withContiguousNDArray old_particle_positions $ \p_old_particle_positions ->
    withContiguousNDArray old_particle_separations $ \p_old_particle_separations ->
    withContiguousNDArray updated_particle_positions $ \p_updated_particle_positions ->
    withContiguousNDArray updated_particle_separations $ \p_updated_particle_separations ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) $ \p_new_particle_positions ->
    withNewNDArray (shape3 number_of_slices number_of_particles number_of_particles) $ \p_new_particle_separations ->
        vpi__path__update_path
            number_of_slices
            number_of_particles
            number_of_dimensions
            update_start_slice
            update_end_slice
            p_old_particle_positions
            p_old_particle_separations
            p_updated_particle_positions
            p_updated_particle_separations
            p_new_particle_positions
            p_new_particle_separations
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape old_particle_positions
    (number_of_slices_to_update :. _ :. _ :. () :: Vec3 Int) = ndarrayShape updated_particle_positions
    update_end_slice = update_start_slice + number_of_slices_to_update - 1
-- @-node:gcross.20100111122429.1492:update_path
-- @-others
-- @-node:gcross.20091217090302.1306:@thin Path.hs
-- @-leo
