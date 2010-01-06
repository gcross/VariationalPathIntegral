-- @+leo-ver=4-thin
-- @+node:gcross.20091216150502.2169:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091216150502.2170:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
-- @nonl
-- @-node:gcross.20091216150502.2170:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091216150502.2171:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Applicative

import Data.NDArray
import qualified Data.NDArray.Listlike as N
import Data.Vec((:.)(..))


import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import System.IO.Unsafe

import VPI.Fortran.Observables
import VPI.Fortran.Path
import VPI.Fortran.Path.Moves
import VPI.Fortran.Physics.HarmonicOscillator
import VPI.Path
-- @-node:gcross.20091216150502.2171:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091226065853.2232:Classes
-- @+node:gcross.20091226065853.2234:AlmostEq
infix 4 ~=

class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = abs (x-y) < 1e-7

instance (AlmostEq a) => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

-- @+at
--  instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
--      (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)
-- @-at
-- @@c

x /~ y = not (x ~= y)
-- @-node:gcross.20091226065853.2234:AlmostEq
-- @-node:gcross.20091226065853.2232:Classes
-- @+node:gcross.20091225065853.1430:Functions
-- @+node:gcross.20091225065853.1431:echo
echo x = trace (show x) x
-- @-node:gcross.20091225065853.1431:echo
-- @+node:gcross.20091226065853.1311:echoWithHeading
echoWithHeading heading x = trace (heading ++ show x) x
-- @-node:gcross.20091226065853.1311:echoWithHeading
-- @+node:gcross.20091226065853.1465:verifyCorrectSeparations
verifyCorrectSeparations particle_positions particle_separations =
    and [   sqrt (sum [(particle_positions ! i3 i j1 k - particle_positions ! i3 i j2 k)**2
                      | k <- [0..number_of_dimensions-1]
                      ]
                 )
            ==
            particle_separations ! i3 i j1 j2
        | i <- [0..number_of_slices-1]
        , j1 <- [0..number_of_particles-1]
        , j2 <- [0..number_of_particles-1]
        ]
  where
    number_of_slices :. number_of_particles :. number_of_dimensions :. () = ndarrayShape particle_positions
-- @-node:gcross.20091226065853.1465:verifyCorrectSeparations
-- @+node:gcross.20091227115154.1366:toSingleton/fromSingleton
toSingleton x = [x]
fromSingleton [x] = x
-- @-node:gcross.20091227115154.1366:toSingleton/fromSingleton
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
    -- @+node:gcross.20091226065853.1624:Fortran wrappers
    [testGroup "Fortran wrappers"
        -- @    @+others
        -- @+node:gcross.20091226065853.1629:vpif.path
        [testGroup "vpif.path"
            -- @    @+others
            -- @+node:gcross.20091226065853.2237:compute_separations
            [testGroup "compute_separations"
                -- @    @+others
                -- @+node:gcross.20091226065853.1631:correct shape
                [testProperty "correct shape" $
                    \(UTI number_of_slices) (UTI number_of_particles) (UTI number_of_dimensions) ->
                        arbitraryNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) (arbitrary :: Gen Double) >>=
                        \particle_positions ->
                            return $ ndarrayShape (compute_separations particle_positions) == shape3 number_of_slices number_of_particles number_of_particles
                -- @-node:gcross.20091226065853.1631:correct shape
                -- @+node:gcross.20091226065853.2236:correct values
                ,testProperty "correct values" $
                    \(UTI number_of_slices) (UTI number_of_particles) (UTI number_of_dimensions) ->
                        arbitraryNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) (arbitrary :: Gen Double) >>=
                        \particle_positions ->
                            return $ verifyCorrectSeparations particle_positions (compute_separations particle_positions)
                -- @-node:gcross.20091226065853.2236:correct values
                -- @-others
                ]
            -- @-node:gcross.20091226065853.2237:compute_separations
            -- @-others
            ]
        -- @-node:gcross.20091226065853.1629:vpif.path
        -- @+node:gcross.20091227115154.1331:vpif.path.moves
        ,testGroup "vpif.path.moves"
            -- @    @+others
            -- @+node:gcross.20091227115154.1334:rigid
            [testGroup "rigid"
                -- @    @+others
                -- @+node:gcross.20091227115154.1333:only selected particle is moved
                [testProperty "only selected particle is moved" $ do
                    number_of_slices <- choose (1,10)
                    number_of_particles <- choose (2,10)
                    number_of_dimensions <- choose (1,10)
                    particle_number <- choose (1,number_of_particles)
                    maximum_shift <- fmap ((+1e-10).abs) arbitrary
                    old_particle_positions <- arbitraryNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) (arbitrary :: Gen Double)
                    let new_particle_positions = unsafePerformIO $ rigid particle_number maximum_shift old_particle_positions
                    return $
                        and [ new_particle_positions ! i3 i j k == old_particle_positions ! i3 i j k
                        | i <- [0..number_of_slices-1]
                        , j <- [0..number_of_particles-1]
                        , j /= (particle_number-1)
                        , k <- [0..number_of_dimensions-1]
                        ]
                        &&
                        and [ new_particle_positions ! i3 i (particle_number-1) k /= old_particle_positions ! i3 i (particle_number-1) k
                        | i <- [0..number_of_slices-1]
                        , k <- [0..number_of_dimensions-1]
                        ]
                -- @-node:gcross.20091227115154.1333:only selected particle is moved
                -- @+node:gcross.20091227115154.1336:moves stay within range
                ,testProperty "only selected particle is moved" $ do
                    number_of_slices <- choose (1,10)
                    let number_of_particles = 1
                        particle_number = 1
                    number_of_dimensions <- choose (1,10)
                    maximum_shift <- fmap abs arbitrary
                    old_particle_positions <- arbitraryNDArray (shape3 number_of_slices number_of_particles number_of_dimensions) (arbitrary :: Gen Double)
                    let new_particle_positions = unsafePerformIO $ rigid particle_number maximum_shift old_particle_positions
                    return $
                        and [ abs (new_particle_positions ! i3 i 0 k - old_particle_positions ! i3 i 0 k) <= 2*maximum_shift
                        | i <- [0..number_of_slices-1]
                        , k <- [0..number_of_dimensions-1]
                        ]
                -- @-node:gcross.20091227115154.1336:moves stay within range
                -- @-others
                ]
            -- @-node:gcross.20091227115154.1334:rigid
            -- @-others
            ]
        -- @-node:gcross.20091227115154.1331:vpif.path.moves
        -- @+node:gcross.20091227115154.1357:vpif.physics.harmonic_oscillator
        ,testGroup "vpif.physics.harmonic_oscillator"
            -- @    @+others
            -- @+node:gcross.20091227115154.1358:compute_potential
            [testGroup "compute_potential"
                -- @    @+others
                -- @+node:gcross.20091227115154.1359:correct monotonicity
                [testProperty "correct monotonicity" $
                    \(Positive (distance_1 :: Double)) (Positive (distance_2 :: Double)) (Positive (coefficient :: Double)) ->
                        let potentialOf =
                                (\[x] -> x)
                                .
                                toList
                                .
                                compute_potential (fromListWithShape (shape1 1) [coefficient])
                                .
                                fromListWithShape (shape3 1 1 1)
                                .
                                (:[])
                        in potentialOf distance_1 < potentialOf (distance_1+distance_2)
                -- @-node:gcross.20091227115154.1359:correct monotonicity
                -- @+node:gcross.20091227115154.1362:correct value
                ,testGroup "correct value"
                    -- @    @+others
                    -- @+node:gcross.20091227115154.1361:1 particle, 1D
                    [testProperty "1 particle, 1D" $
                        \(Positive (distance :: Double)) (Positive (coefficient :: Double)) ->
                            let potentialOf =
                                    fromSingleton
                                    .
                                    toList
                                    .
                                    compute_potential (fromList [coefficient])
                                    .
                                    fromListWithShape (shape3 1 1 1)
                                    .
                                    toSingleton
                            in potentialOf distance == coefficient * distance**2 / 2
                    -- @-node:gcross.20091227115154.1361:1 particle, 1D
                    -- @+node:gcross.20091227115154.1364:1 particle, ND
                    ,testProperty "1 particle, ND" $
                        \(distances_and_coefficients :: [(Double,Double)]) ->
                            let (distances,coefficients) = unzip distances_and_coefficients
                            in  (== sum [distance**2 * coefficient / 2 | (distance,coefficient) <- distances_and_coefficients])
                                .
                                fromSingleton
                                .
                                toList
                                .
                                compute_potential (fromList coefficients)
                                .
                                fromListWithShape (shape3 1 1 (length distances))
                                $
                                distances
                    -- @-node:gcross.20091227115154.1364:1 particle, ND
                    -- @-others
                    ]
                -- @nonl
                -- @-node:gcross.20091227115154.1362:correct value
                -- @-others
                ]
            -- @-node:gcross.20091227115154.1358:compute_potential
            -- @-others
            ]
        -- @-node:gcross.20091227115154.1357:vpif.physics.harmonic_oscillator
        -- @-others
        ]
    -- @-node:gcross.20091226065853.1624:Fortran wrappers
    -- @+node:gcross.20091216150502.2173:Path
    ,testGroup "Path"
        -- @    @+others
        -- @+node:gcross.20091216150502.2174:createInitialPath
        [testGroup "createInitialPath"
            -- @    @+others
            -- @+node:gcross.20091226065853.1313:correct shape
            [testProperty "correct shape" $
                \(UTI number_of_slices) (UTI number_of_particles) unprocessed_bounds -> (not.null) unprocessed_bounds ==>
                let bounds = [if bound_1 < bound_2 then (bound_1,bound_2) else (bound_2,bound_1) | (bound_1,bound_2) <- unprocessed_bounds]
                    number_of_dimensions = length bounds
                in unsafePerformIO $
                    createInitialPath
                        number_of_slices
                        number_of_particles
                        bounds
                    >>=
                    return
                    .
                    \Path
                    {   pathLength = path_number_of_slices
                    ,   pathNumberOfParticles = path_number_of_particles
                    ,   pathNumberOfDimensions = path_number_of_dimensions
                    ,   pathParticlePositions = particle_positions
                    ,   pathParticleSeparations = particle_separations
                    } ->
                        and [path_number_of_slices == number_of_slices
                            ,path_number_of_particles == number_of_particles
                            ,path_number_of_dimensions == number_of_dimensions
                            ,ndarrayShape particle_positions == number_of_slices :. number_of_particles :. number_of_dimensions :. ()
                            ,ndarrayShape particle_separations == number_of_slices :. number_of_particles :. number_of_particles :. ()
                            ]
            -- @-node:gcross.20091226065853.1313:correct shape
            -- @+node:gcross.20091226065853.1317:within specified range
            ,testProperty "within specified range" $
                \(UTI number_of_slices) (UTI number_of_particles) unprocessed_bounds -> (not.null) unprocessed_bounds ==>
                let bounds = [if bound_1 < bound_2 then (bound_1,bound_2) else (bound_2,bound_1) | (bound_1,bound_2) <- unprocessed_bounds]
                in unsafePerformIO $
                    createInitialPath
                        number_of_slices
                        number_of_particles
                        bounds
                    >>=
                    \Path { pathParticlePositions = particle_positions } ->
                        return
                        .
                        any (
                            \(index,(lower_bound,upper_bound)) ->
                                N.all ((>= lower_bound) <^(&&)^> (<= upper_bound) )
                                .
                                cut (All :. All :. Index index :. ())
                                $
                                particle_positions
                        )
                        $
                        zip [0..] bounds
            -- @-node:gcross.20091226065853.1317:within specified range
            -- @+node:gcross.20091226065853.1315:correct separations
            ,testProperty "correct separations" $
                \(UTI number_of_slices) (UTI number_of_particles) unprocessed_bounds -> (not.null) unprocessed_bounds ==>
                let bounds = [if bound_1 < bound_2 then (bound_1,bound_2) else (bound_2,bound_1) | (bound_1,bound_2) <- unprocessed_bounds]
                in unsafePerformIO $
                    createInitialPath
                        number_of_slices
                        number_of_particles
                        bounds
                    >>=
                    return
                    .
                    liftA2 verifyCorrectSeparations pathParticlePositions pathParticleSeparations
            -- @-node:gcross.20091226065853.1315:correct separations
            -- @-others
            ]
        -- @-node:gcross.20091216150502.2174:createInitialPath
        -- @-others
        ]
    -- @-node:gcross.20091216150502.2173:Path
    -- @+node:gcross.20100105133218.1558:Physics
    ,testGroup "Physics"
        -- @    @+others
        -- @+node:gcross.20100105133218.1559:Harmonic Oscillator
        [testGroup "Harmonic Oscillator"
            -- @    @+others
            -- @+node:gcross.20100105133218.1564:correct energy
            [testGroup "correct energy"
                -- @    @+others
                -- @+node:gcross.20100105133218.1563:single particle, single dimension, unit coefficient
                [testProperty "single particle, single dimension, unit coefficient" $ do
                    particle_positions_3darray <- arbitraryNDArray (shape3 1 1 1) (choose (0,1))
                    let coefficients = fromList [1]
                        particle_positions_2darray = cut (Index 0 :. All :. All :. ()) particle_positions_3darray
                        potential = compute_potential coefficients particle_positions_3darray ! (0 :. ())
                        (gradient_of_log_trial_fn,laplacian_of_log_trial_fn) = compute_trial_derivatives coefficients particle_positions_2darray
                    return $ 0.5 ~= (compute_energy 0.5 potential gradient_of_log_trial_fn laplacian_of_log_trial_fn)
                -- @-node:gcross.20100105133218.1563:single particle, single dimension, unit coefficient
                -- @+node:gcross.20100105133218.1570:single particle, single dimension, random coefficient
                ,testProperty "single particle, single dimension, random coefficient" $ do
                    particle_positions_3darray <- arbitraryNDArray (shape3 1 1 1) (choose (0,1))
                    Positive coefficient <- arbitrary
                    let coefficients = fromList [coefficient]
                        particle_positions_2darray = cut (Index 0 :. All :. All :. ()) particle_positions_3darray
                        potential = compute_potential coefficients particle_positions_3darray ! (0 :. ())
                        (gradient_of_log_trial_fn,laplacian_of_log_trial_fn) = compute_trial_derivatives coefficients particle_positions_2darray
                    return $ (0.5*coefficient) ~= (compute_energy 0.5 potential gradient_of_log_trial_fn laplacian_of_log_trial_fn)
                -- @-node:gcross.20100105133218.1570:single particle, single dimension, random coefficient
                -- @+node:gcross.20100105133218.1572:single particle, multiple dimension, random coefficients
                ,testProperty "single particle, multiple dimensions, random coefficients" $ do
                    number_of_dimensions <- choose (1,10)
                    particle_positions_3darray <- arbitraryNDArray (shape3 1 1 number_of_dimensions) (choose (0,1))
                    coefficients <- fmap (fromList . map abs) (vectorOf number_of_dimensions (choose (0,1)))
                    let particle_positions_2darray = cut (Index 0 :. All :. All :. ()) particle_positions_3darray
                        potential = compute_potential coefficients particle_positions_3darray ! (0 :. ())
                        (gradient_of_log_trial_fn,laplacian_of_log_trial_fn) = compute_trial_derivatives coefficients particle_positions_2darray
                    return $ (0.5*N.sum coefficients) ~= (compute_energy 0.5 potential gradient_of_log_trial_fn laplacian_of_log_trial_fn)
                -- @-node:gcross.20100105133218.1572:single particle, multiple dimension, random coefficients
                -- @+node:gcross.20100105133218.1576:single particle, multiple dimension, random coefficients
                ,testProperty "multiple particles, multiple dimensions, random coefficients" $ do
                    number_of_dimensions <- choose (1,10)
                    number_of_particles <- choose (1,10)
                    particle_positions_3darray <- arbitraryNDArray (shape3 1 number_of_particles number_of_dimensions) (choose (0,1))
                    coefficients <- fmap (fromList . map abs) (vectorOf number_of_dimensions (choose (0,1)))
                    let particle_positions_2darray = cut (Index 0 :. All :. All :. ()) particle_positions_3darray
                        potential = compute_potential coefficients particle_positions_3darray ! (0 :. ())
                        (gradient_of_log_trial_fn,laplacian_of_log_trial_fn) = compute_trial_derivatives coefficients particle_positions_2darray
                    return $
                        (0.5*(fromIntegral number_of_particles)*N.sum coefficients)
                        ~=
                        (compute_energy 0.5 potential gradient_of_log_trial_fn laplacian_of_log_trial_fn)
                -- @-node:gcross.20100105133218.1576:single particle, multiple dimension, random coefficients
                -- @-others
                ]
            -- @-node:gcross.20100105133218.1564:correct energy
            -- @-others
            ]
        -- @-node:gcross.20100105133218.1559:Harmonic Oscillator
        -- @-others
        ]
    -- @-node:gcross.20100105133218.1558:Physics
    -- @-others
    -- @-node:gcross.20091216150502.2172:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091216150502.2169:@thin test.hs
-- @-leo
