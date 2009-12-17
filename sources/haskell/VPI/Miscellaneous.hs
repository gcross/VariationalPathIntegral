-- @+leo-ver=4-thin
-- @+node:gcross.20091216150502.2164:@thin Miscellaneous.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091216150502.2188:<< Language extensions >>
-- @-node:gcross.20091216150502.2188:<< Language extensions >>
-- @nl

module VPI.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20091216150502.2167:<< Import needed modules >>
import Control.Exception

import Data.StorableVector.Base (Vector(..))

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
-- @-node:gcross.20091216150502.2167:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091216150502.2165:Functions
-- @+node:gcross.20091216150502.2166:withNewVector
withNewVector :: (Storable a) => Int -> (Ptr a -> IO b) -> IO (Vector a,b)
withNewVector size thunk = do
    vector_fptr <- mallocForeignPtrArray size
    result <- withForeignPtr vector_fptr thunk
    vector <- evaluate (SV vector_fptr 0 size)
    return (vector,result)

-- @-node:gcross.20091216150502.2166:withNewVector
-- @-node:gcross.20091216150502.2165:Functions
-- @-others
-- @-node:gcross.20091216150502.2164:@thin Miscellaneous.hs
-- @-leo
