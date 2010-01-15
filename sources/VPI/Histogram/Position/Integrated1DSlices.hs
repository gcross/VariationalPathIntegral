-- @+leo-ver=4-thin
-- @+node:gcross.20100114153410.1599:@thin Integrated1DSlices.hs
-- @@language Haskell

module VPI.Histogram.Position.Integrated1DSlices where

-- @<< Import needed modules >>
-- @+node:gcross.20100114153410.1607:<< Import needed modules >>
import Control.Monad

import Data.NDArray
import Data.NDArray.Classes
import Data.NDArray.Cuts
import Data.NDArray.Indexable
import Data.NDArray.Mutable
import Data.Int
import Data.Vec ((:.)(..))

import VPI.Fortran.Histograms.Position
import VPI.Path
import VPI.Physics
import VPI.Spliceable
-- @-node:gcross.20100114153410.1607:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100114153410.1605:Types
-- @+node:gcross.20100114153410.1606:Histogram
data Histogram = Histogram
    {   histogramData :: MutableArray2D Int32
    ,   histogramLowerBounds :: Array1D Double
    ,   histogramUpperBounds :: Array1D Double
    }
-- @-node:gcross.20100114153410.1606:Histogram
-- @-node:gcross.20100114153410.1605:Types
-- @+node:gcross.20100114153410.1608:Functions
-- @+node:gcross.20100114153410.1609:createEmptyHistogram
createEmptyHistogram :: Int -> [(Double,Double)] -> IO Histogram
createEmptyHistogram number_of_bins bounds =
    createFromListWithShape (shape2 number_of_dimensions number_of_bins) (repeat 0)
    >>=
    \histogram_data ->
        return $
            Histogram
                histogram_data
                (fromList lower_bounds)
                (fromList upper_bounds)
  where
    (lower_bounds,upper_bounds) = unzip bounds
    number_of_dimensions = length bounds
-- @-node:gcross.20100114153410.1609:createEmptyHistogram
-- @+node:gcross.20100114153410.1860:updateHistogram
updateHistogram :: Histogram -> PathSlice -> IO ()
updateHistogram histogram path_slice =
    bin_all_1d_integrated_slices
        (histogramLowerBounds histogram)
        (histogramUpperBounds histogram)
        (histogramData histogram)
        (pathSliceParticlePositions path_slice)
-- @-node:gcross.20100114153410.1860:updateHistogram
-- @+node:gcross.20100114153410.2338:updateHistogramFromConfiguration
updateHistogramFromConfiguration :: Histogram -> Int -> Configuration -> IO ()
updateHistogramFromConfiguration histogram slice_number =
    updateHistogram histogram
    .
    slice slice_number
    .
    configurationPath
-- @-node:gcross.20100114153410.2338:updateHistogramFromConfiguration
-- @+node:gcross.20100114153410.1862:summarizeHistogram
summarizeHistogram :: Histogram -> IO [[(Double,Double)]]
summarizeHistogram histogram =
    forM (zip3 [0..number_of_bins-1] lower_bounds upper_bounds) $
        \(slice_number,lower_bound,upper_bound) ->
            readIntoList (cut (Index slice_number :. ()) (histogramData histogram))
            >>=
            \counts ->
                let normalization_factor = recip . fromIntegral . sum $ counts
                    bin_width = (upper_bound - lower_bound) / fromIntegral number_of_bins
                    first_bin = lower_bound + bin_width/2
                    last_bin = first_bin + fromIntegral (number_of_bins-1) * bin_width
                    bins = [first_bin,first_bin+bin_width..last_bin]
                in return $
                    [ (bin,fromIntegral count*normalization_factor)
                    | (bin,count) <- zip bins counts
                    ]
  where
    _ :. number_of_bins :. () = ndarrayShape . histogramData $ histogram
    lower_bounds = toList . histogramLowerBounds $ histogram
    upper_bounds = toList . histogramUpperBounds $ histogram
-- @-node:gcross.20100114153410.1862:summarizeHistogram
-- @+node:gcross.20100114153410.2337:writeHistogramToFiles
writeHistogramToFiles :: [FilePath] -> Histogram -> IO ()
writeHistogramToFiles filepaths =
    summarizeHistogram
    >=>
    mapM_ (
        \(filepath,histogram) ->
            writeFile filepath
            .
            unlines
            $
            [show bin ++ " " ++ show count | (bin,count) <- histogram]
    ) . zip filepaths
-- @-node:gcross.20100114153410.2337:writeHistogramToFiles
-- @-node:gcross.20100114153410.1608:Functions
-- @-others
-- @-node:gcross.20100114153410.1599:@thin Integrated1DSlices.hs
-- @-leo
