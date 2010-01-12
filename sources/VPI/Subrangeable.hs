-- @+leo-ver=4-thin
-- @+node:gcross.20100111122429.2025:@thin Subrangeable.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100111122429.2026:<< Language extensions >>
-- @-node:gcross.20100111122429.2026:<< Language extensions >>
-- @nl

module VPI.Subrangeable where

-- @<< Import needed modules >>
-- @+node:gcross.20100111122429.2027:<< Import needed modules >>
-- @-node:gcross.20100111122429.2027:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100111122429.2028:Classes
-- @+node:gcross.20100111122429.2029:Subrangeable
class Subrangeable a where
    subrange :: Int -> Int -> a -> a
-- @-node:gcross.20100111122429.2029:Subrangeable
-- @-node:gcross.20100111122429.2028:Classes
-- @-others
-- @-node:gcross.20100111122429.2025:@thin Subrangeable.hs
-- @-leo
