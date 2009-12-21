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
    {   pathLength :: Int
    ,   pathNumberOfParticles :: Int
    ,   pathNumberOfDimensions :: Int
    ,   pathParticlePositions :: NDArray (Vec3 Int) Double
    ,   pathParticleSeparations :: NDArray (Vec3 Int) Double
    }
-- @-node:gcross.20091211140304.1696:Path
-- @-node:gcross.20091211140304.1695:Types
-- @+node:gcross.20091216150502.1731:Functions
-- @+node:gcross.20091216150502.1732:createInitialPath
createInitialPath :: Int -> Int -> [(Double,Double)] -> IO Path
createInitialPath number_of_slices number_of_particles bounds =
    create_initial_path number_of_slices number_of_particles bounds
    >>=
    \(particle_positions,particle_separations) ->
        return $
        Path
            number_of_slices
            number_of_particles
            (length bounds)
            particle_positions
            particle_separations
-- @-node:gcross.20091216150502.1732:createInitialPath
-- @-node:gcross.20091216150502.1731:Functions
-- @-others
-- @-node:gcross.20091211140304.1694:@thin Path.hs
-- @-leo
