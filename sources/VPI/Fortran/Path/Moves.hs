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
import Data.Vec ((:.)(..)
                ,Vec2
                ,Vec3
                ,Vec4
                ,Vec5
                ,Vec6
                ,Vec7
                ,Vec8
                ,Vec9
                )

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

rigid :: Int -> Double -> Array3D Double -> IO (Array3D Double)
rigid particle_number maximum_shift old_particle_positions =
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
-- @-others
-- @-node:gcross.20091227115154.1326:@thin Moves.hs
-- @-leo
