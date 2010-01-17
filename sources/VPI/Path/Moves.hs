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
brownianBridgeMove :: Bool -> Double -> Double -> Int -> Int -> Int -> Path -> IO (Int,Path)
brownianBridgeMove
    duplicate_center_slice
    hbar_over_2m time_interval
    particle_number
    start_slice end_slice
    old_path
    =
    let number_of_slices = pathNumberOfSlices old_path
        (start_of_cut_,end_of_cut_,endpoint_move_mode) =
            case (start_slice > 0, end_slice < number_of_slices) of
                (True,True) -> (start_slice-1,end_slice+1,MoveNeitherEndpoint)
                (False,True) -> (0,end_slice+1,MoveLeftEndpoint)
                (True,False) -> (start_slice-1,number_of_slices,MoveRightEndpoint)
                (False,False) -> error "brownian bridge can only operator on a subsection of the path, not the whole thing"
        center_slice_number = number_of_slices `div` 2 - 1
        start_of_cut =
            if start_of_cut_ >= center_slice_number && start_of_cut_ <= center_slice_number+1
                then center_slice_number-1
                else start_of_cut_
        end_of_cut =
            if end_of_cut_ >= center_slice_number && end_of_cut_ <= center_slice_number+1
                then center_slice_number+2
                else end_of_cut_
        center_slice_mode =
            if start_of_cut < center_slice_number && end_of_cut > center_slice_number+1
                then if duplicate_center_slice
                        then DuplicatedCenterSlice
                        else DisconnectedCenterSlice
                else NoCenterSlice
        Path old_positions old_separations =
            subrange start_of_cut end_of_cut old_path
    in
    brownian_bridge
        hbar_over_2m time_interval
        (particle_number+1)
        endpoint_move_mode
        center_slice_mode
        (center_slice_number+1)
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
-- @-node:gcross.20100114153410.1570:Functions
-- @-others
-- @-node:gcross.20100107114651.1444:@thin Moves.hs
-- @-leo
