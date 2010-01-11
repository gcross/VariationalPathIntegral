-- @+leo-ver=4-thin
-- @+node:gcross.20091211140304.1694:@thin Path.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217090302.1337:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @nonl
-- @-node:gcross.20091217090302.1337:<< Language extensions >>
-- @nl

module VPI.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20091211140304.1697:<< Import needed modules >>
import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.Vec (Vec3,(:.)(..),get)

import VPI.Fortran.Path
import VPI.Updatable
-- @nonl
-- @-node:gcross.20091211140304.1697:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211140304.1695:Types
-- @+node:gcross.20091211140304.1696:Path
data Path = Path
    {   pathParticlePositions :: Array3D Double
    ,   pathParticleSeparations :: Array3D Double
    }
-- @-node:gcross.20091211140304.1696:Path
-- @+node:gcross.20100107114651.1435:PathSlice
data PathSlice = PathSlice
    {   pathSliceNumber :: Int
    ,   pathSliceParticlePositions :: Array2D Double
    ,   pathSliceParticleSeparations :: Array2D Double
    }
-- @-node:gcross.20100107114651.1435:PathSlice
-- @-node:gcross.20091211140304.1695:Types
-- @+node:gcross.20100111122429.1486:Instances
-- @+node:gcross.20100111122429.1487:Updatable
instance Updatable Path where
    update (Path old_particle_positions old_particle_separations) update_start_slice (Path updated_particle_positions updated_particle_separations) =
        uncurry Path $ update_path old_particle_positions old_particle_separations update_start_slice updated_particle_positions updated_particle_separations
-- @-node:gcross.20100111122429.1487:Updatable
-- @-node:gcross.20100111122429.1486:Instances
-- @+node:gcross.20091216150502.1731:Functions
-- @+node:gcross.20091216150502.1732:createInitialPath
createInitialPath :: Int -> Int -> [(Double,Double)] -> IO Path
createInitialPath number_of_slices number_of_particles bounds =
    fmap (uncurry Path) $ create_initial_path number_of_slices number_of_particles bounds
-- @-node:gcross.20091216150502.1732:createInitialPath
-- @+node:gcross.20100107114651.1448:computeSeparations
computeSeparations = compute_separations
-- @-node:gcross.20100107114651.1448:computeSeparations
-- @+node:gcross.20100107114651.1449:makePathFromPositions
makePathFromPositions particle_positions =
    Path
        {   pathParticlePositions = particle_positions
        ,   pathParticleSeparations = computeSeparations particle_positions
        }
-- @-node:gcross.20100107114651.1449:makePathFromPositions
-- @+node:gcross.20100106124611.2083:(queries)
pathNumberOfSlices :: Path -> Int
pathNumberOfSlices (Path particle_positions _) = number_of_slices
  where
    (number_of_slices :. _ :. _ :. _ :: Vec3 Int) = ndarrayShape particle_positions

pathLength :: Path -> Int
pathLength = pathNumberOfSlices

pathNumberOfParticles :: Path -> Int
pathNumberOfParticles (Path particle_positions _) = number_of_particles
  where
    (_ :. number_of_particles :. _ :. _ :: Vec3 Int) = ndarrayShape particle_positions

pathNumberOfDimensions :: Path -> Int
pathNumberOfDimensions (Path particle_positions _) = number_of_dimensions
  where
    (_ :. _ :. number_of_dimensions :. _ :: Vec3 Int) = ndarrayShape particle_positions
-- @-node:gcross.20100106124611.2083:(queries)
-- @+node:gcross.20100107114651.1436:slicePath
slicePath :: Int -> Path -> PathSlice
slicePath slice_number path
  | slice_number < 0
    = error $ "Negative slice number: " ++ show slice_number
  | slice_number >= pathNumberOfSlices path
    = error $ "Slice number is greater than size of the path: " ++ show slice_number ++ " > " ++ show (pathNumberOfSlices path)
  | otherwise
    = let slice = Index slice_number :. All :. All :. ()
      in PathSlice
            {   pathSliceNumber = slice_number
            ,   pathSliceParticlePositions = cut slice (pathParticlePositions path)
            ,   pathSliceParticleSeparations = cut slice (pathParticleSeparations path)
            }
-- @-node:gcross.20100107114651.1436:slicePath
-- @-node:gcross.20091216150502.1731:Functions
-- @-others
-- @-node:gcross.20091211140304.1694:@thin Path.hs
-- @-leo
