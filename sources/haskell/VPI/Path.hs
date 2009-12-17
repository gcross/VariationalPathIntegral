-- @+leo-ver=4-thin
-- @+node:gcross.20091211140304.1694:@thin Path.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091216150502.2190:<< Language extensions >>
{-# LANGUAGE ForeignFunctionInterface #-}
-- @-node:gcross.20091216150502.2190:<< Language extensions >>
-- @nl

module VPI.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20091211140304.1697:<< Import needed modules >>
import Data.StorableVector (Vector)

import Foreign.Marshal.Array
import Foreign.Ptr

import VPI.Miscellaneous
-- @-node:gcross.20091211140304.1697:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211140304.1695:Types
-- @+node:gcross.20091211140304.1696:Path
data Path = Path
    {   pathLength :: Int
    ,   pathNumberOfParticles :: Int
    ,   pathNumberOfDimensions :: Int
    ,   pathParticlePositions :: Vector Double
    ,   pathParticleSeparations :: Vector Double
    }
-- @-node:gcross.20091211140304.1696:Path
-- @-node:gcross.20091211140304.1695:Types
-- @+node:gcross.20091216150502.1731:Functions
-- @+node:gcross.20091216150502.1732:createInitialPath
foreign import ccall unsafe "vpi__path__create_initial_path" vpi__path__create_initial_path :: 
    Int -> -- number of dimensions
    Int -> -- number of particles
    Int -> -- number of slices
    Ptr (Double) -> -- lower bounds
    Ptr (Double) -> -- upper bounds
    Ptr (Double) -> -- particle positions
    Ptr (Double) -> -- particle slices
    IO ()

createInitialPath :: Int -> Int -> Int -> [(Double,Double)] -> IO Path
createInitialPath number_of_slices number_of_particles number_of_dimensions bounds
 | length bounds /= number_of_dimensions
    = error $
        "The length of the bounds array ("
        ++ show (length bounds) ++
        ") does not match the number of dimensions ("
        ++ show number_of_dimensions ++
        ")"
 | otherwise
    = ( withArray lower_bounds $ \p_lower_bounds ->
        withArray upper_bounds $ \p_upper_bounds ->
        withNewVector (number_of_slices*number_of_particles*number_of_dimensions) $ \p_positions ->
        withNewVector (number_of_slices*number_of_particles*number_of_particles) $ \p_separations ->
            vpi__path__create_initial_path
                number_of_dimensions
                number_of_particles
                number_of_slices
                p_lower_bounds
                p_upper_bounds
                p_positions
                p_separations
      )  >>=
        \(positions,(separations,())) ->
            return $
                Path
                    number_of_slices
                    number_of_particles
                    number_of_dimensions
                    positions
                    separations
  where
    (lower_bounds,upper_bounds) = unzip bounds
-- @-node:gcross.20091216150502.1732:createInitialPath
-- @-node:gcross.20091216150502.1731:Functions
-- @-others
-- @-node:gcross.20091211140304.1694:@thin Path.hs
-- @-leo
