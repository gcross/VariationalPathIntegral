-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1444:@thin Moves.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100114153410.1568:<< Language extensions >>
-- @-node:gcross.20100114153410.1568:<< Language extensions >>
-- @nl

module VPI.Path.Moves where

-- @<< Import needed modules >>
-- @+node:gcross.20100114153410.1569:<< Import needed modules >>
import VPI.Fortran.Path.Moves
import VPI.Path
-- @-node:gcross.20100114153410.1569:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100114153410.1570:Functions
-- @+node:gcross.20100114153410.1571:rigidMove
rigidMove :: Double -> Int -> Path -> IO Path
rigidMove maximum_shift particle_number =
    fmap makePathFromPositions
    .
    rigid (particle_number+1) maximum_shift
    .
    pathParticlePositions
-- @-node:gcross.20100114153410.1571:rigidMove
-- @-node:gcross.20100114153410.1570:Functions
-- @-others
-- @-node:gcross.20100107114651.1444:@thin Moves.hs
-- @-leo
