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
import Control.Arrow

import Data.Function
import Data.List
import Data.NDArray
import Data.NDArray.Cuts
import Data.NDArray.Classes
import Data.NDArray.Indexable
import qualified Data.NDArray.Listlike as N
import Data.NDArray.Mutable
import Data.Number.Erf
import Data.Vec((:.)(..))

import Debug.Trace

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.AntiTest
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.Statistics
import Test.QuickCheck

import System.Random.Mersenne
import System.IO.Unsafe

import VPI.Fortran.GreensFunction.SecondOrder
import VPI.Fortran.Histograms.Position
import VPI.Fortran.Observables
import VPI.Fortran.Path
import VPI.Fortran.Path.Moves
import VPI.Fortran.Physics.HarmonicOscillator
import VPI.Estimator
import VPI.Observable
import VPI.Observable.Energy
import VPI.Path
import VPI.Physics
import VPI.Physics.HarmonicOscillator
import VPI.Sliceable
import VPI.Subrangeable
import VPI.Thermalize
import VPI.Updatable
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
        -- @+node:gcross.20100106124611.2011:vpif.greens_function
        [testGroup "vpif.greens_function"
            -- @    @+others
            -- @+node:gcross.20100106124611.2012:2nd order
            [testGroup "2nd order"
                -- @    @+others
                -- @+node:gcross.20100106124611.2013:initialize_weights
                [testProperty "initialize_weights" $
                    choose (2,10) >>= return .
                    (
                        \number_of_slices ->
                            toList (initialize_weights number_of_slices) == [0.5] ++ replicate (number_of_slices-2) 1.0 ++ [0.5]
                    )
                -- @-node:gcross.20100106124611.2013:initialize_weights
                -- @+node:gcross.20100106124611.2017:compute_log_greens_function
                ,testProperty "compute_log_greens_function" $
                    liftA2 (~=)
                        (sum . map (uncurry (*)))
                        (uncurry compute_log_greens_function . (fromList *** fromList) . unzip)
                -- @-node:gcross.20100106124611.2017:compute_log_greens_function
                -- @-others
                ]
            -- @-node:gcross.20100106124611.2012:2nd order
            -- @-others
            ]
        -- @-node:gcross.20100106124611.2011:vpif.greens_function
        -- @+node:gcross.20100109140101.1559:vpif.histograms.position
        ,testGroup "vpif.histograms.position"
            -- @    @+others
            -- @+node:gcross.20100109140101.1560:bin_all_1d_integrated_slices
            [testProperty "bin_all_1d_integrated_slices" $ do
                number_of_dimensions <- choose (1,10)
                number_of_bins <- choose (2,10)
                lower_bounds <- vectorOf number_of_dimensions (choose (-2,-1))
                upper_bounds <- vectorOf number_of_dimensions (choose (1,2))
                particle_position <- mapM choose (zip lower_bounds upper_bounds)
                let particle_positions_ndarray = fromListWithShape (shape2 1 number_of_dimensions) particle_position
                    lower_bounds_ndarray = fromList lower_bounds
                    upper_bounds_ndarray = fromList upper_bounds
                    histogram = unsafePerformIO $ do
                        histogram <- createFromListWithShape (shape2 number_of_dimensions number_of_bins) (repeat 0)
                        bin_all_1d_integrated_slices lower_bounds_ndarray upper_bounds_ndarray histogram particle_positions_ndarray
                        readIntoList histogram
                    bins = zipWith3
                            (\position lower_bound upper_bound ->
                                floor $ (position-lower_bound)/(upper_bound-lower_bound)*fromIntegral number_of_bins
                            )
                            particle_position lower_bounds upper_bounds 
                    correct_histogram = concat [replicate bin 0 ++ [1] ++ replicate (number_of_bins-bin-1) 0 | bin <- bins]
                return $ correct_histogram == histogram
            -- @-node:gcross.20100109140101.1560:bin_all_1d_integrated_slices
            -- @-others
            ]
        -- @-node:gcross.20100109140101.1559:vpif.histograms.position
        -- @+node:gcross.20091226065853.1629:vpif.path
        ,testGroup "vpif.path"
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
                ,testProperty "moves stay within range" $ do
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
                -- @+node:gcross.20100109140101.1475:uniform distribution
                ,testDistribution "uniform distribution" id 10000 0.001 $ fmap ((0.5 -) . (! i3 0 0 0)) $ rigid 1 0.5 (fromListWithShape (shape3 1 1 1) [0])
                -- @-node:gcross.20100109140101.1475:uniform distribution
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
            -- @+node:gcross.20100106124611.2023:compute_trial_weight
            ,testGroup "compute_trial_weight"
                -- @    @+others
                -- @+node:gcross.20100106124611.2024:correct sign
                [testProperty "correct sign" $
                    \(distance :: Double) (Positive (coefficient :: Double)) ->
                        let weightOf =
                                compute_trial_weight (fromListWithShape (shape1 1) [coefficient])
                                .
                                fromListWithShape (shape2 1 1)
                                .
                                (:[])
                        in weightOf distance <= 0
                -- @-node:gcross.20100106124611.2024:correct sign
                -- @+node:gcross.20100106124611.2031:correct monotonicity
                ,testProperty "correct monotonicity" $
                    \(Positive (distance_1 :: Double)) (Positive (distance_2 :: Double)) (Positive (coefficient :: Double)) ->
                        let weightOf =
                                compute_trial_weight (fromListWithShape (shape1 1) [coefficient])
                                .
                                fromListWithShape (shape2 1 1)
                                .
                                (:[])
                        in weightOf distance_1 >= weightOf (distance_1+distance_2)
                -- @-node:gcross.20100106124611.2031:correct monotonicity
                -- @-others
                ]
            -- @-node:gcross.20100106124611.2023:compute_trial_weight
            -- @-others
            ]
        -- @-node:gcross.20091227115154.1357:vpif.physics.harmonic_oscillator
        -- @-others
        ]
    -- @-node:gcross.20091226065853.1624:Fortran wrappers
    -- @+node:gcross.20100111215927.1535:Estimator
    ,testGroup "Estimator"
        -- @    @+others
        -- @+node:gcross.20100111215927.1536:correct statistics
        [testProperty "correct statistics" $
            \(samples :: [Double]) -> (not . null) samples ==>
                let number_of_samples = fromIntegral (length samples)
                    correct_mean = sum samples / number_of_samples
                    correct_variance = (sum . map (**2)) samples / number_of_samples - correct_mean**2
                    (mean,variance) = (summarizeEstimator . foldl' updateEstimator emptyEstimator) samples --'
                in mean == correct_mean && variance == correct_variance
        -- @-node:gcross.20100111215927.1536:correct statistics
        -- @-others
        ]
    -- @-node:gcross.20100111215927.1535:Estimator
    -- @+node:gcross.20100111215927.1546:Observables
    ,testGroup "Observables"
        -- @    @+others
        -- @+node:gcross.20100111215927.1547:Energy
        [testProperty "Energy" $ do
            number_of_slices <- choose (2,5)
            number_of_particles <- choose (1,5)
            number_of_dimensions <- choose (1,3)
            number_of_paths <- choose (1,10)
            let coefficients = fromList (replicate number_of_dimensions 1)
            configurations <- vectorOf number_of_paths
                              .
                              fmap (makeConfigurationFromPath (computePotential coefficients))
                              $
                              arbitraryPath number_of_slices number_of_particles number_of_dimensions (choose (0,1))
            let (mean,variance) = unsafePerformIO $ do
                    Observable updateObservable summarizeObservable
                        <- createEnergyObservable
                            0.5
                            (computeTrialDerivatives coefficients)
                    mapM_ updateObservable configurations
                    summarizeObservable
            return $
                mean == (fromIntegral number_of_particles*fromIntegral number_of_dimensions*0.5)
                &&
                variance == 0
        -- @-node:gcross.20100111215927.1547:Energy
        -- @-others
        ]
    -- @-node:gcross.20100111215927.1546:Observables
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
                    \(Path particle_positions particle_separations) ->
                        and [ndarrayShape particle_positions == number_of_slices :. number_of_particles :. number_of_dimensions :. ()
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
        -- @+node:gcross.20100111122429.1997:update (Path)
        ,testProperty "update (Path)" $ do
            number_of_slices <- choose (4,10)
            number_of_particles <- choose (1,10)
            number_of_dimensions <- choose (1,10)
            start_slice <- choose (0,number_of_slices-2)
            end_slice <- choose (start_slice+1,number_of_slices-1)
            let number_of_slices_to_update = end_slice-start_slice+1
            Path old_particle_positions old_particle_separations
                <- arbitraryPath number_of_slices number_of_particles number_of_dimensions arbitrary
            Path updated_particle_positions updated_particle_separations
                <- arbitraryPath number_of_slices_to_update number_of_particles number_of_dimensions arbitrary
            let Path new_particle_positions new_particle_separations =
                    update (Path old_particle_positions old_particle_separations)
                           start_slice
                           (Path updated_particle_positions updated_particle_separations)
                correct_particle_positions_list =
                    (if start_slice > 0
                        then toList . cut (Range 0 start_slice :. All :. All :. ()) $ old_particle_positions
                        else []
                    )
                    ++
                    (toList updated_particle_positions)
                    ++
                    (if end_slice < number_of_slices-1
                        then toList . cut (Range (end_slice+1) number_of_slices :. All :. All :. ()) $ old_particle_positions
                        else []
                    )
                correct_particle_separations_list =
                    (if start_slice > 0
                        then toList . cut (Range 0 start_slice :. All :. All :. ()) $ old_particle_separations
                        else []
                    )
                    ++
                    (toList updated_particle_separations)
                    ++
                    (if end_slice < number_of_slices-1
                        then toList . cut (Range (end_slice+1) number_of_slices :. All :. All :. ()) $ old_particle_separations
                        else []
                    )
            return $
                (correct_particle_positions_list == toList new_particle_positions)
                &&
                (correct_particle_separations_list == toList new_particle_separations)
        -- @-node:gcross.20100111122429.1997:update (Path)
        -- @-others
        ]
    -- @-node:gcross.20091216150502.2173:Path
    -- @+node:gcross.20100105133218.1558:Physics
    ,testGroup "Physics"
        -- @    @+others
        -- @+node:gcross.20100105133218.1559:Harmonic Oscillator
        [testGroup "Harmonic Oscillator"
            -- @    @+others
            -- @+node:gcross.20100112190325.1562:correct potential
            [testProperty "correct potential" $ do
                number_of_slices <- choose (2,5)
                number_of_particles <- choose (1,5)
                number_of_dimensions <- choose (1,3)
                coefficients <- arbitraryNDArray (shape1 number_of_dimensions) (choose (0,1))
                path <- arbitraryPath number_of_slices number_of_particles number_of_dimensions (choose (0,1))
                let potential_as_list = toList . unwrapPotential . computePotential coefficients $ path
                    correct_potential_as_list =
                        [   (/2)
                            .
                            sum
                            .
                            zipWith (*) (toList coefficients)
                            $
                            [   sum
                                .
                                map (\x -> x*x)
                                .
                                toList
                                .
                                cut (Index slice_index :. All :. Index dimension_index :. ())
                                .
                                pathParticlePositions
                                $
                                path
                            |dimension_index <- [0..number_of_dimensions-1]
                            ]
                       |slice_index <- [0..number_of_slices-1]
                       ]
                return $ potential_as_list == correct_potential_as_list
            -- @-node:gcross.20100112190325.1562:correct potential
            -- @+node:gcross.20100111215927.1839:correct energy
            ,testGroup "correct energy"
                -- @    @+others
                -- @+node:gcross.20100105133218.1564:via Fortran correct_energy function
                [testGroup "via Fortran correct_energy function"
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
                    -- @+node:gcross.20100105133218.1576:multiple particles, multiple dimension, random coefficients
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
                    -- @-node:gcross.20100105133218.1576:multiple particles, multiple dimension, random coefficients
                    -- @-others
                    ]
                -- @-node:gcross.20100105133218.1564:via Fortran correct_energy function
                -- @+node:gcross.20100111215927.1840:via computeEnergyAtSlice
                ,testProperty "via computeEnergyAtSlice" $ do
                    number_of_slices <- choose (2,5)
                    number_of_particles <- choose (1,5)
                    number_of_dimensions <- choose (1,3)
                    let coefficients = fromList (replicate number_of_dimensions 1)
                    configuration <- fmap (makeConfigurationFromPath (computePotential coefficients))
                                     $
                                     arbitraryPath number_of_slices number_of_particles number_of_dimensions (choose (0,1))
                    let energies =
                            map (computeEnergyAtSlice 0.5 (computeTrialDerivatives coefficients)
                                 .
                                 flip slice configuration
                                )
                                [0..number_of_slices-1]
                        correct_energy =0.5 * fromIntegral (number_of_particles*number_of_dimensions)

                    return $ all (~= correct_energy) energies
                -- @-node:gcross.20100111215927.1840:via computeEnergyAtSlice
                -- @-others
                ]
            -- @-node:gcross.20100111215927.1839:correct energy
            -- @-others
            ]
        -- @-node:gcross.20100105133218.1559:Harmonic Oscillator
        -- @-others
        ]
    -- @-node:gcross.20100105133218.1558:Physics
    -- @+node:gcross.20100107114651.1453:Thermalize
    ,testGroup "Thermalize"
        -- @    @+others
        -- @+node:gcross.20100107114651.1455:decideWhetherToAcceptChange
        [testGroup "decideWhetherToAcceptChange"
            -- @    @+others
            -- @+node:gcross.20100107114651.1456:new_weight > old_weight
            [testProperty "new_weight > old_weight" $
                \weight_1 weight_2 ->
                    let old_weight = weight_1 `min` weight_2
                        new_weight = weight_1 `max` weight_2
                    in unsafePerformIO (decideWhetherToAcceptChange old_weight new_weight)
            -- @-node:gcross.20100107114651.1456:new_weight > old_weight
            -- @+node:gcross.20100107114651.1473:new_weight - old_weight = log 0.5
            ,testBernoulli "new_weight - old_weight = log 0.5" 0.5 0.1 0.001 $ decideWhetherToAcceptChange 0 (log 0.5)
            -- @-node:gcross.20100107114651.1473:new_weight - old_weight = log 0.5
            -- @+node:gcross.20100107114651.1479:new_weight - old_weight = log 0.1
            ,testBernoulli "new_weight - old_weight = log 0.1" 0.1 0.1 0.001 $ decideWhetherToAcceptChange 0 (log 0.1)
            -- @-node:gcross.20100107114651.1479:new_weight - old_weight = log 0.1
            -- @+node:gcross.20100109140101.1529:effectively samples linear distribution
            -- @+at
            --  ,testGroup "effectively samples linear distribution" $
            --      let computeNextPosition previous_position = do
            --              next_position <- randomIO
            --              accept <- decideWhetherToAcceptChange (log 
            --  previous_position) (log next_position)
            --              return . (id &&& id) $
            --                  if accept
            --                      then next_position
            --                      else previous_position
            --      in  [testWalkDistribution "cumulative distribution = x^2 
            --  (correct, should succeed)" (\x -> x*x) 40000 0.001 (return 0) 
            --  computeNextPosition
            --          ,antiTest $ testWalkDistribution "cumulative 
            --  distribution = x^2.1 (false, should fail)" (\x -> x**2.1) 
            --  40000 0.01 (return 0) computeNextPosition
            --          ]
            -- @-at
            -- @@c
            -- @-node:gcross.20100109140101.1529:effectively samples linear distribution
            -- @+node:gcross.20100109140101.1533:effectively samples harmonic oscillator ground state
            -- @+at
            --  ,testGroup "effectively samples harmonic oscillator ground 
            --  state" $
            --      let computeWeight = (2*) . compute_trial_weight (fromList 
            --  [1]) . fromListWithShape (shape2 1 1) . (:[])
            --          computeNextPosition previous_position = do
            --              next_position <- fmap 
            --  ((previous_position+).(0.5-).(*1)) randomIO
            --              accept <- decideWhetherToAcceptChange 
            --  (computeWeight previous_position) (computeWeight 
            --  next_position)
            --              return . (id &&& id) $
            --                  if accept
            --                      then next_position
            --                      else previous_position
            --      in  [testWalkDistribution "cumulative distribution = erf 
            --  (correct, should succeed)" ((/2).(+1).erf) 40000 0.0001 
            --  (return 0) computeNextPosition
            --          ,antiTest $ testWalkDistribution "cumulative 
            --  distribution = erf*1.1 (correct, should succeed)" 
            --  ((**1.1).(/2).(+1).erf) 40000 0.001 (return 0) 
            --  computeNextPosition
            --          ]
            -- @-at
            -- @@c
            -- @-node:gcross.20100109140101.1533:effectively samples harmonic oscillator ground state
            -- @-others
            ]
        -- @-node:gcross.20100107114651.1455:decideWhetherToAcceptChange
        -- @-others
        ]
    -- @-node:gcross.20100107114651.1453:Thermalize
    -- @+node:gcross.20100111122429.1746:Updatable
    ,testGroup "Updatable"
        -- @    @+others
        -- @+node:gcross.20100111122429.1750:updateArray
        [testProperty "updateArray" $ do
            array_length <- choose (4,20)
            start_index <- choose (0,array_length-2)
            end_index <- choose (start_index+1,array_length-1)
            let update_length = end_index-start_index+1
            old_array <- arbitraryNDArray (shape1 array_length) (arbitrary :: Gen Int)
            updated_array <- arbitraryNDArray (shape1 update_length) arbitrary
            let new_array = fst . unsafePerformIO $
                    withNDArray old_array $ \p_old_array ->
                    withNDArray updated_array $ \p_updated_array ->
                    withNewNDArray (ndarrayShape old_array) $ \p_new_array ->
                        updateArray array_length p_old_array update_length p_updated_array start_index p_new_array
                correct_list =
                    (if start_index > 0
                        then toList . cut (Range 0 start_index :. ()) $ old_array
                        else []
                    )
                    ++
                    (toList updated_array)
                    ++
                    (if end_index < array_length-1
                        then toList . cut (Range (end_index+1) array_length :. ()) $ old_array
                        else []
                    )
            return $ (correct_list == toList new_array)
        -- @-node:gcross.20100111122429.1750:updateArray
        -- @+node:gcross.20100111122429.2005:update
        ,testGroup "update"
            -- @    @+others
            -- @+node:gcross.20100111122429.2002:Array1D
            [testProperty "Array1D" $ do
                number_of_slices <- choose (4,10)
                start_slice <- choose (0,number_of_slices-2)
                end_slice <- choose (start_slice+1,number_of_slices-1)
                let number_of_slices_to_update = end_slice-start_slice+1
                old_ndarray <- arbitraryNDArray (shape1 number_of_slices) (arbitrary :: Gen Double)
                updated_ndarray <- arbitraryNDArray (shape1 number_of_slices_to_update) arbitrary
                let new_ndarray = update old_ndarray start_slice updated_ndarray
                    correct_list =
                        (if start_slice > 0
                            then toList . cut (Range 0 start_slice :. ()) $ old_ndarray
                            else []
                        )
                        ++
                        (toList updated_ndarray)
                        ++
                        (if end_slice < number_of_slices-1
                            then toList . cut (Range (end_slice+1) number_of_slices :. ()) $ old_ndarray
                            else []
                        )

                return $ (correct_list == toList new_ndarray)
            -- @-node:gcross.20100111122429.2002:Array1D
            -- @+node:gcross.20100111122429.2004:Array2D
            ,testProperty "Array2D" $ do
                number_of_slices <- choose (4,10)
                size2 <- choose (1,5)
                start_slice <- choose (0,number_of_slices-2)
                end_slice <- choose (start_slice+1,number_of_slices-1)
                let number_of_slices_to_update = end_slice-start_slice+1
                old_ndarray <- arbitraryNDArray (shape2 number_of_slices size2) (arbitrary :: Gen Double)
                updated_ndarray <- arbitraryNDArray (shape2 number_of_slices_to_update size2) arbitrary
                let new_ndarray = update old_ndarray start_slice updated_ndarray
                    correct_list =
                        (if start_slice > 0
                            then toList . cut (Range 0 start_slice :. All :. ()) $ old_ndarray
                            else []
                        )
                        ++
                        (toList updated_ndarray)
                        ++
                        (if end_slice < number_of_slices-1
                            then toList . cut (Range (end_slice+1) number_of_slices :. All :. ()) $ old_ndarray
                            else []
                        )

                return $ (correct_list == toList new_ndarray)
            -- @-node:gcross.20100111122429.2004:Array2D
            -- @+node:gcross.20100111122429.2007:Array3D
            ,testProperty "Array3D" $ do
                number_of_slices <- choose (4,10)
                size2 <- choose (1,5)
                size3 <- choose (1,5)
                start_slice <- choose (0,number_of_slices-2)
                end_slice <- choose (start_slice+1,number_of_slices-1)
                let number_of_slices_to_update = end_slice-start_slice+1
                old_ndarray <- arbitraryNDArray (shape3 number_of_slices size2 size3) (arbitrary :: Gen Double)
                updated_ndarray <- arbitraryNDArray (shape3 number_of_slices_to_update size2 size3) arbitrary
                let new_ndarray = update old_ndarray start_slice updated_ndarray
                    correct_list =
                        (if start_slice > 0
                            then toList . cut (Range 0 start_slice :. All :. All :. ()) $ old_ndarray
                            else []
                        )
                        ++
                        (toList updated_ndarray)
                        ++
                        (if end_slice < number_of_slices-1
                            then toList . cut (Range (end_slice+1) number_of_slices :. All :. All :. ()) $ old_ndarray
                            else []
                        )

                return $ (correct_list == toList new_ndarray)
            -- @-node:gcross.20100111122429.2007:Array3D
            -- @-others
            ]
        -- @-node:gcross.20100111122429.2005:update
        -- @-others
        ]
    -- @-node:gcross.20100111122429.1746:Updatable
    -- @-others
    -- @-node:gcross.20091216150502.2172:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091216150502.2169:@thin test.hs
-- @-leo
