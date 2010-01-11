-- @+leo-ver=4-thin
-- @+node:gcross.20100111122429.1484:@thin Updatable.hs
-- @@language Haskell

module VPI.Updatable where

-- @+others
-- @+node:gcross.20100111122429.1485:Updatable
class Updatable a where
    update :: a -> Int -> a -> a
-- @-node:gcross.20100111122429.1485:Updatable
-- @-others
-- @-node:gcross.20100111122429.1484:@thin Updatable.hs
-- @-leo
