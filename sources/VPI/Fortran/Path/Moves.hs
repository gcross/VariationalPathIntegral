-- @+leo-ver=4-thin
-- @+node:gcross.20091227115154.1326:@thin Moves.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091227115154.1327:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20091227115154.1327:<< Language extensions >>
-- @nl
-- @<< Link dependencies >>
-- @+node:gcross.20100105133218.1363:<< Link dependencies >>
{-# BLUEPRINT-LINK-DEPENDENCY vpic.path.moves o #-}
{-# BLUEPRINT-LINK-DEPENDENCY vpif.path.moves o #-}
-- @-node:gcross.20100105133218.1363:<< Link dependencies >>
-- @nl

module VPI.Fortran.Path.Moves where

-- @<< Import needed modules >>
-- @+node:gcross.20091227115154.1328:<< Import needed modules >>
import Control.Arrow

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import Data.Vec ((:.)(..))

import Foreign.Marshal.Array
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20091227115154.1328:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091227115154.1329:rigid
foreign import ccall unsafe "vpic__path__moves__rigid" vpi__path__moves__rigid :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Int -> -- particle number to shift
    Double -> -- maximum shift
    Ptr (Double) -> -- old particle positions
    Ptr (Double) -> -- new particle positions
    IO ()

rigid :: Double -> Int -> Array3D Double -> IO (Array3D Double)
rigid maximum_shift particle_number old_particle_positions =
    fmap fst $
    withContiguousNDArray old_particle_positions $ \p_old_particle_positions ->
    withNewNDArray (ndarrayShape old_particle_positions) $ \p_new_particle_positions ->
        vpi__path__moves__rigid
            number_of_slices
            number_of_particles
            number_of_dimensions
            particle_number
            maximum_shift
            p_old_particle_positions
            p_new_particle_positions
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape old_particle_positions
-- @-node:gcross.20091227115154.1329:rigid
-- @+node:gcross.20100116114537.1618:brownian_bridge
foreign import ccall unsafe "vpic__path__moves__brownian_bridge" vpi__path__moves__brownian_bridge :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Int -> -- particle number to shift
    Bool -> -- move leftmost slice
    Bool -> -- move rightmost slice
    Double -> -- hbar/2m
    Double -> -- length of time step
    Ptr (Double) -> -- old particle positions
    Ptr (Double) -> -- new particle positions
    IO ()

brownian_bridge ::
    Double -> Double ->
    Int ->
    Bool -> Bool ->
    Array3D Double ->
    IO (Array3D Double)
brownian_bridge
    hbar_over_2m time_interval
    particle_number
    move_leftmost_slice move_rightmost_slice
    old_particle_positions
    =
    fmap fst $
    withContiguousNDArray old_particle_positions $ \p_old_particle_positions ->
    withNewNDArray (ndarrayShape old_particle_positions) $ \p_new_particle_positions ->
        vpi__path__moves__brownian_bridge
            number_of_slices
            number_of_particles
            number_of_dimensions
            particle_number
            move_leftmost_slice
            move_rightmost_slice
            hbar_over_2m
            time_interval
            p_old_particle_positions
            p_new_particle_positions
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape old_particle_positions
-- @-node:gcross.20100116114537.1618:brownian_bridge
-- @+node:gcross.20100116114537.2100:linked_brownian_bridge
foreign import ccall unsafe "vpic__path__moves__linked_brownian_bridge" vpi__path__moves__linked_brownian_bridge :: 
    Int -> -- number of slices
    Int -> -- number of particles
    Int -> -- number of dimensions
    Int -> -- particle number to shift
    Bool -> -- move leftmost slice
    Bool -> -- move rightmost slice
    Int -> -- link slice number
    Bool -> -- duplicate link slice
    Double -> -- hbar/2m
    Double -> -- length of time step
    Ptr (Double) -> -- old particle positions
    Ptr (Double) -> -- new particle positions
    IO ()

linked_brownian_bridge ::
    Bool ->
    Double -> Double ->
    Int ->
    Int ->
    Bool -> Bool ->
    Array3D Double ->
    IO (Array3D Double)
linked_brownian_bridge
    duplicate_link_slice
    hbar_over_2m time_interval
    link_slice_number
    particle_number
    move_leftmost_slice move_rightmost_slice
    old_particle_positions
    =
    fmap fst $
    withContiguousNDArray old_particle_positions $ \p_old_particle_positions ->
    withNewNDArray (ndarrayShape old_particle_positions) $ \p_new_particle_positions ->
        vpi__path__moves__linked_brownian_bridge
            number_of_slices
            number_of_particles
            number_of_dimensions
            particle_number
            move_leftmost_slice
            move_rightmost_slice
            link_slice_number
            duplicate_link_slice
            hbar_over_2m
            time_interval
            p_old_particle_positions
            p_new_particle_positions
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape old_particle_positions
-- @-node:gcross.20100116114537.2100:linked_brownian_bridge
-- @-others
-- @-node:gcross.20091227115154.1326:@thin Moves.hs
-- @-leo
