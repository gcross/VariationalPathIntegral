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
import VPI.Spliceable
-- @-node:gcross.20100114153410.1569:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100114153410.1570:Functions
-- @+node:gcross.20100114153410.1571:rigidMove
rigidMove :: Double -> Int -> Path -> IO Path
rigidMove maximum_shift particle_number (Path old_positions old_separations) =
    rigid maximum_shift (particle_number+1) old_positions
    >>=
    \new_positions ->
        return $
            Path
                new_positions
                (updateSeparationsForParticle particle_number old_positions old_separations)
-- @-node:gcross.20100114153410.1571:rigidMove
-- @+node:gcross.20100116114537.1623:brownianBridgeMove
brownianBridgeMove :: Double -> Double -> Int -> Int -> Int -> Path -> IO (Int,Path)
brownianBridgeMove
    hbar_over_2m time_interval
    particle_number
    start_slice end_slice
    old_path
    =
    let number_of_slices = pathNumberOfSlices old_path
        (start_of_cut,move_leftmost_slice) =
            if start_slice <= 0
                then (0,True)
                else (start_slice-1,False)
        (end_of_cut,move_rightmost_slice) =
            if end_slice >= number_of_slices
                then (number_of_slices,True)
                else (end_slice+1,False)
        Path old_positions old_separations =
            subrange start_of_cut end_of_cut old_path
    in
    brownian_bridge
        hbar_over_2m time_interval
        (particle_number+1)
        move_leftmost_slice
        move_rightmost_slice
        old_positions
    >>=
    \new_positions ->
        return $
            (start_of_cut
            ,Path
                new_positions
                (updateSeparationsForParticle particle_number old_positions old_separations)
            )
-- @-node:gcross.20100116114537.1623:brownianBridgeMove
-- @+node:gcross.20100116114537.2102:linkedBrownianBridgeMove
linkedBrownianBridgeMove ::
    Bool ->
    Double -> Double ->
    Int ->
    Int ->
    Int -> Int ->
    Path ->
    IO (Int,Path)
linkedBrownianBridgeMove
    duplicate_link_slice
    hbar_over_2m time_interval
    link_slice_number
    particle_number
    start_slice end_slice
    old_path
  | end_slice < link_slice_number || start_slice > link_slice_number+1
    = brownianBridgeMove
        hbar_over_2m time_interval
        particle_number
        start_slice end_slice
        old_path
  | otherwise
    = 
    let number_of_slices = pathNumberOfSlices old_path
        (start_of_cut,move_leftmost_slice) =
            if start_slice <= 0
                then (0,True)
                else (start_slice-1,False)
        (end_of_cut,move_rightmost_slice) =
            if end_slice >= number_of_slices
                then (number_of_slices,True)
                else (end_slice+1,False)
        Path old_positions old_separations =
            subrange start_of_cut end_of_cut old_path
    in
    linked_brownian_bridge
        duplicate_link_slice
        hbar_over_2m time_interval
        (link_slice_number-start_slice+1)
        (particle_number+1)
        move_leftmost_slice
        move_rightmost_slice
        old_positions
    >>=
    \new_positions ->
        return $
            (start_of_cut
            ,Path
                new_positions
                (updateSeparationsForParticle particle_number old_positions old_separations)
            )
-- @-node:gcross.20100116114537.2102:linkedBrownianBridgeMove
-- @-node:gcross.20100114153410.1570:Functions
-- @-others
-- @-node:gcross.20100107114651.1444:@thin Moves.hs
-- @-leo
