-- @+leo-ver=4-thin
-- @+node:gcross.20091211140304.1694:@thin Path.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091217090302.1337:<< Language extensions >>
-- @-node:gcross.20091217090302.1337:<< Language extensions >>
-- @nl

module VPI.Path where

-- @<< Import needed modules >>
-- @+node:gcross.20091211140304.1697:<< Import needed modules >>
import Data.NDArray
import Data.Vec (Vec3,(:.)(..))

import VPI.Fortran.Path
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
-- @-node:gcross.20091211140304.1695:Types
-- @+node:gcross.20091216150502.1731:Functions
-- @+node:gcross.20091216150502.1732:createInitialPath
createInitialPath :: Int -> Int -> [(Double,Double)] -> IO Path
createInitialPath number_of_slices number_of_particles bounds =
    fmap (uncurry Path) $ create_initial_path number_of_slices number_of_particles bounds
-- @-node:gcross.20091216150502.1732:createInitialPath
-- @+node:gcross.20100106124611.2083:(queries)
pathNumberOfSlices :: Path -> Int
pathNumberOfSlices (Path particle_positions _) = number_of_slices
  where
    (number_of_slices :. _ :. _ :. _) = ndarrayShape particle_positions

pathLength :: Path -> Int
pathLength = pathNumberOfSlices

pathNumberOfParticles :: Path -> Int
pathNumberOfParticles (Path particle_positions _) = number_of_particles
  where
    (_ :. number_of_particles :. _ :. _) = ndarrayShape particle_positions

pathNumberOfDimensions :: Path -> Int
pathNumberOfDimensions (Path particle_positions _) = number_of_dimensions
  where
    (_ :. _ :. number_of_dimensions :. _) = ndarrayShape particle_positions
-- @-node:gcross.20100106124611.2083:(queries)
-- @-node:gcross.20091216150502.1731:Functions
-- @-others
-- @-node:gcross.20091211140304.1694:@thin Path.hs
-- @-leo
