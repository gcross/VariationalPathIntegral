-- @+leo-ver=4-thin
-- @+node:gcross.20091216150502.2169:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091216150502.2170:<< Language extensions >>
-- @-node:gcross.20091216150502.2170:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091216150502.2171:<< Import needed modules >>
import Control.Applicative.Infix


import Data.NDArray (All(..),Index(..))
import qualified Data.NDArray as N
import Data.Vec((:.)(..))


import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import System.IO.Unsafe

import VPI.Path
-- @-node:gcross.20091216150502.2171:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091225065853.1430:Functions
-- @+node:gcross.20091225065853.1431:echo
echo x = trace (show x) x
-- @-node:gcross.20091225065853.1431:echo
-- @+node:gcross.20091225065853.1432:skipList
skipList :: Int -> [a] -> [a]
skipList _ [] = []
skipList n (x:xs) = x:skipList n (drop (n-1) xs)
-- @-node:gcross.20091225065853.1432:skipList
-- @-node:gcross.20091225065853.1430:Functions
-- @+node:gcross.20091216150502.2182:Generators
-- @+node:gcross.20091216150502.2183:UnderTenInt
newtype UnderTenInt = UTI Int deriving (Show,Eq)
instance Arbitrary UnderTenInt where
    arbitrary = choose (1,10) >>= return.UTI
-- @-node:gcross.20091216150502.2183:UnderTenInt
-- @+node:gcross.20091216150502.2186:PhysicalDimensionInt
newtype PhysicalDimensionInt = PDI Int deriving (Show,Eq)
instance Arbitrary PhysicalDimensionInt where
    arbitrary = choose (2,4) >>= return.PDI
-- @-node:gcross.20091216150502.2186:PhysicalDimensionInt
-- @-node:gcross.20091216150502.2182:Generators
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091216150502.2172:<< Tests >>
    -- @+others
    -- @+node:gcross.20091216150502.2173:VPI.Path
    [testGroup "VMPS.Path"
        -- @    @+others
        -- @+node:gcross.20091216150502.2174:createInitialPath
        [testGroup "createInitialPath"
            -- @    @+others
            -- @+node:gcross.20091216150502.2175:within specified range (1D)
            [testProperty "within specified range (1D)" $
                \(UTI number_of_slices) (UTI number_of_particles) bound_1 bound_2 ->
                let lower_bound = bound_1 `min` bound_2
                    upper_bound = bound_1 `max` bound_2
                in unsafePerformIO $
                    createInitialPath
                        number_of_slices
                        number_of_particles
                        [(lower_bound,upper_bound)]
                    >>=
                    return . N.all ( (>= lower_bound) <^(&&)^> (<= upper_bound) ) . pathParticlePositions
            -- @-node:gcross.20091216150502.2175:within specified range (1D)
            -- @+node:gcross.20091220132355.1796:within specified range (2D)
            ,testProperty "within specified range (2D)" $
                \(UTI number_of_slices) (UTI number_of_particles) bound_x_1 bound_x_2 bound_y_1 bound_y_2 ->
                let lower_bound_x = bound_x_1 `min` bound_x_2
                    upper_bound_x = bound_x_1 `max` bound_x_2
                    lower_bound_y = bound_y_1 `min` bound_y_2
                    upper_bound_y = bound_y_1 `max` bound_y_2
                in unsafePerformIO $
                    createInitialPath
                        number_of_slices
                        number_of_particles
                        [(lower_bound_x,upper_bound_x)
                        ,(lower_bound_y,upper_bound_y)
                        ]
                    >>=
                    return .
                      (
                        N.all ( (>= lower_bound_x) <^(&&)^> (<= upper_bound_x) )
                        .
                        N.cut (All :. All :. Index 0 :. ())
                        .
                        pathParticlePositions
                      <^(&&)^>
                        N.all ( (>= lower_bound_y) <^(&&)^> (<= upper_bound_y) )
                        .
                        N.cut (All :. All :. Index 1 :. ())
                        .
                        pathParticlePositions
                      )
            -- @-node:gcross.20091220132355.1796:within specified range (2D)
            -- @-others
            ]
        -- @-node:gcross.20091216150502.2174:createInitialPath
        -- @-others
        ]
    -- @-node:gcross.20091216150502.2173:VPI.Path
    -- @-others
    -- @-node:gcross.20091216150502.2172:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091216150502.2169:@thin test.hs
-- @-leo
