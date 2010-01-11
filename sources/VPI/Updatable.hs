-- @+leo-ver=4-thin
-- @+node:gcross.20100111122429.1484:@thin Updatable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100111122429.2000:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20100111122429.2000:<< Language extensions >>
-- @nl

module VPI.Updatable where

-- @<< Import needed modules >>
-- @+node:gcross.20100111122429.1751:<< Import needed modules >>
import Control.Exception

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Indexable
import qualified Data.Vec as V

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr

import System.IO.Unsafe
-- @-node:gcross.20100111122429.1751:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111122429.1745:Classes
-- @+node:gcross.20100111122429.1485:Updatable
class Updatable a where
    update :: a -> Int -> a -> a
-- @-node:gcross.20100111122429.1485:Updatable
-- @-node:gcross.20100111122429.1745:Classes
-- @+node:gcross.20100111122429.1998:Instances
-- @+node:gcross.20100111122429.1999:Updatable (NDArray)
instance (Indexable indexType, Storable dataType, V.Fold indexType Int, V.Head indexType Int) => Updatable (NDArray indexType dataType) where
    update old_ndarray update_start_slice updated_ndarray =
        assert (old_stride == update_stride) $
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
-- @-node:gcross.20100111122429.1999:Updatable (NDArray)
-- @-node:gcross.20100111122429.1998:Instances
-- @+node:gcross.20100111122429.1744:updateArray
updateArray :: Storable a => Int -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> IO ()
updateArray old_array_length old_array updated_array_length updated_array update_start_index new_array = do
    copyArray new_array old_array update_start_index
    copyArray start_of_updated_section_in_new updated_array updated_array_length
    copyArray resumption_of_original_section_in_new resumption_of_original_section_in_old (old_array_length-updated_array_length-update_start_index)
  where
    start_of_updated_section_in_new = new_array `advancePtr` update_start_index
    resumption_of_original_section_in_old = old_array `advancePtr` update_start_index `advancePtr` updated_array_length
    resumption_of_original_section_in_new = new_array `advancePtr` update_start_index `advancePtr` updated_array_length
-- @-node:gcross.20100111122429.1744:updateArray
-- @-others
-- @-node:gcross.20100111122429.1484:@thin Updatable.hs
-- @-leo
