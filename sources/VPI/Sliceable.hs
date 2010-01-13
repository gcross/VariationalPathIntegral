-- @+leo-ver=4-thin
-- @+node:gcross.20100111215927.1571:@thin Sliceable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100111215927.1572:<< Language extensions >>
{-# LANGUAGE TypeFamilies #-}
-- @nonl
-- @-node:gcross.20100111215927.1572:<< Language extensions >>
-- @nl

module VPI.Sliceable where

-- @<< Import needed modules >>
-- @+node:gcross.20100111215927.1573:<< Import needed modules >>
-- @-node:gcross.20100111215927.1573:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111215927.1574:Classes
-- @+node:gcross.20100111215927.1575:Sliceable
class Sliceable a where
    type SliceResult a
    slice :: Int -> a -> (SliceResult a)
    numberOfSlices :: a -> Int
-- @-node:gcross.20100111215927.1575:Sliceable
-- @-node:gcross.20100111215927.1574:Classes
-- @+node:gcross.20100111215927.1576:Functions
-- @+node:gcross.20100111215927.1577:firstSlice / lastSLice
firstSlice, lastSlice :: Sliceable a => a -> SliceResult a
firstSlice = slice 0
lastSlice x = slice (numberOfSlices x - 1) x
-- @-node:gcross.20100111215927.1577:firstSlice / lastSLice
-- @-node:gcross.20100111215927.1576:Functions
-- @-others
-- @-node:gcross.20100111215927.1571:@thin Sliceable.hs
-- @-leo
