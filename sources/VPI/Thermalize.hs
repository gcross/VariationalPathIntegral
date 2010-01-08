-- @+leo-ver=4-thin
-- @+node:gcross.20100107114651.1450:@thin Thermalize.hs
-- @@language Haskell

module VPI.Thermalize where

-- @<< Import needed modules >>
-- @+node:gcross.20100107114651.1452:<< Import needed modules >>
import System.Random
-- @-node:gcross.20100107114651.1452:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100107114651.1451:decideWhetherToAcceptChange
decideWhetherToAcceptChange :: Double -> Double -> IO Bool
decideWhetherToAcceptChange old_weight new_weight = fmap (< exp (new_weight-old_weight)) randomIO
-- @-node:gcross.20100107114651.1451:decideWhetherToAcceptChange
-- @-others
-- @-node:gcross.20100107114651.1450:@thin Thermalize.hs
-- @-leo
