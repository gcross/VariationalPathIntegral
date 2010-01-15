-- @+leo-ver=4-thin
-- @+node:gcross.20100111215927.1571:@thin Spliceable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100111215927.1572:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- @nonl
-- @-node:gcross.20100111215927.1572:<< Language extensions >>
-- @nl

module VPI.Spliceable where

-- @<< Import needed modules >>
-- @+node:gcross.20100111215927.1573:<< Import needed modules >>
import Control.Exception

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import qualified Data.Vec as V

import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe
-- @-node:gcross.20100111215927.1573:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1574:Classes
-- @+node:gcross.20100111215927.1575:Spliceable
class Spliceable a where
    type SliceResult a
    slice :: Int -> a -> (SliceResult a)
    subrange_ :: Int -> Int -> a -> a
    update :: a -> Int -> a -> a
    numberOfSlices :: a -> Int
-- @-node:gcross.20100111215927.1575:Spliceable
-- @-node:gcross.20100111215927.1574:Classes
-- @+node:gcross.20100111215927.1576:Functions
-- @+node:gcross.20100111215927.1577:firstSlice / lastSlice
firstSlice, lastSlice :: Spliceable a => a -> SliceResult a
firstSlice = slice 0
lastSlice x = slice (numberOfSlices x - 1) x
-- @-node:gcross.20100111215927.1577:firstSlice / lastSlice
-- @+node:gcross.20100115122605.1886:subrange
subrange :: Spliceable a => Int -> Int -> a -> a
subrange start_slice end_slice array
    | start_slice == 0 && end_slice == numberOfSlices array
        = array
    | otherwise
        = subrange_ start_slice end_slice array
-- @-node:gcross.20100115122605.1886:subrange
-- @+node:gcross.20100115122605.1883:updateNDArray
updateNDArray ::
    (Show indexType
    ,Indexable indexType
    ,Storable dataType
    ,V.Fold indexType Int
    ,V.Head indexType Int
    ,Eq indexType
    ) =>
    NDArray indexType dataType ->
    Int ->
    NDArray indexType dataType ->
    NDArray indexType dataType
updateNDArray old_ndarray update_start_slice updated_ndarray
   | (old_shape == updated_shape) && (update_start_slice == 0)
      = updated_ndarray
   | otherwise
      = assert (old_stride == update_stride) $
        assert (update_end_slice <= number_of_slices) $
        fst . unsafePerformIO $
        withContiguousNDArray old_ndarray $ \p_old_ndarray ->
        withContiguousNDArray updated_ndarray $ \p_updated_ndarray ->
        withNewNDArray old_shape $ \p_new_ndarray ->
            updateArray old_length p_old_ndarray update_length p_updated_ndarray update_start_index p_new_ndarray
  where
    old_shape = ndarrayShape old_ndarray
    old_length = V.product old_shape
    number_of_slices = V.head old_shape
    old_stride = old_length `div` number_of_slices

    updated_shape = ndarrayShape updated_ndarray
    update_length = V.product updated_shape
    number_of_slices_to_update = V.head updated_shape
    update_stride = update_length `div` number_of_slices_to_update
    update_start_index = update_start_slice * update_stride
    update_end_slice = update_start_slice + number_of_slices_to_update
-- @-node:gcross.20100115122605.1883:updateNDArray
-- @+node:gcross.20100115122605.1885:updateArray
updateArray :: Storable a => Int -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> IO ()
updateArray old_array_length old_array updated_array_length updated_array update_start_index new_array = do
    copyArray new_array old_array update_start_index
    copyArray start_of_updated_section_in_new updated_array updated_array_length
    copyArray resumption_of_original_section_in_new resumption_of_original_section_in_old (old_array_length-updated_array_length-update_start_index)
  where
    start_of_updated_section_in_new = new_array `advancePtr` update_start_index
    resumption_of_original_section_in_old = old_array `advancePtr` update_start_index `advancePtr` updated_array_length
    resumption_of_original_section_in_new = new_array `advancePtr` update_start_index `advancePtr` updated_array_length
-- @-node:gcross.20100115122605.1885:updateArray
-- @-node:gcross.20100111215927.1576:Functions
-- @-others
-- @-node:gcross.20100111215927.1571:@thin Spliceable.hs
-- @-leo
