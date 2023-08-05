{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy   as BL
import           Data.Csv               (FromNamedRecord (parseNamedRecord),
                                         decodeByName, (.:))
import           Data.List              (transpose)
import qualified Data.Map               as M
import qualified Data.Vector            as V
import           System.Environment     (getArgs)
import           Text.PrettyPrint.Boxes

main :: IO ()
main = do

  -- Parse arguments
  -- f1 is the CSV file produced with the new version
  -- f2 is the CSV file produced with the old version
  (f1:f2:v:_) <- getArgs

  -- Parse results. The result of decoding the file is an Either, and the Right
  -- case has a pair (Header, Vector Result). We use snd to keep the Vector.
  csvData1 <- fmap snd <$> decodeByName <$> BL.readFile f1
  csvData2 <- fmap snd <$> decodeByName <$> BL.readFile f2

  -- Obtain the list of violations and correct comparisons by comparing the
  -- data obtained from two CSV files using an auxiliary comparison function
  let correct    = fst <$> result
      violations = snd <$> result

      result = compareResults comparisonFunc <$> csvData1 <*> csvData2

      -- v is the third argument when executing the program. It acts as
      -- a threshold of how much better/worse the second version must be
      -- than the first.
      --
      -- 1 means the old version must be slower or the same as the new one.
      -- 2 means the new version must be at least twice as fast as the new one.
      -- 0.9 means the new version can be up to 10% slower than the old one.
      comparisonFunc x y = x * (read v) <= y

  -- Print results
  putStrLn "Correct"
  printResults correct

  putStrLn "Violations"
  printResults violations

-- | Compare two CSV databases, and produce the correct results and the
-- violations.
compareResults :: (Double -> Double -> Bool)  -- ^ Comparison function
               -> V.Vector Result             -- ^ Data from first file.
               -> V.Vector Result             -- ^ Data from second file.
               -> (M.Map String Comparison, M.Map String Comparison)
compareResults prop rows1 rows2 =
    M.partition (compareDurations prop) combinedCriterionData

  where

    combinedCriterionData = M.unionWith mergeComparisons map1 map2

    -- Turn the result data (a vector) into a map
    map1 = resultsToMapWith mkComparisonFst rows1
    map2 = resultsToMapWith mkComparisonSnd rows2

printResults :: Either String (M.Map String Comparison) -> IO ()
printResults (Left s)  = putStrLn $ "Error: " ++ s
printResults (Right m) = putStrLn table
  where
    table = render $ hsep 3 left $ map (vcat left . map text)
          $ transpose cols
    cols :: [[String]]
    cols = ["Benchmark", "Alt 1", "Alt 2", "Factor (alt2 / alt1)"]
         : [ [ benchmark, alt1, alt2, factor ]
           | (benchmark, comp) <- M.assocs m
           , let alt1 :: String
                 alt1 = maybe "(nothing)" show $ comparisonDuration1 comp
           , let alt2 :: String
                 alt2 = maybe "(nothing)" show $ comparisonDuration2 comp
           , let factor =
                   case (comparisonDuration1 comp, comparisonDuration2 comp) of
                     (Just v1, Just v2) -> show (v2/v1)
                     _                  -> "N/A"
           ]

-- * Comparisons

-- | Comparison entry
data Comparison = Comparison
    { comparisonDuration1 :: Maybe Double
    , comparisonDuration2 :: Maybe Double
    }
  deriving Show

-- | Constructor with 1st comparison value only.
mkComparisonFst :: Double -> Comparison
mkComparisonFst v = Comparison (Just v) Nothing

-- | Constructor with 2nd comparison value only.
mkComparisonSnd :: Double -> Comparison
mkComparisonSnd v = Comparison Nothing (Just v)

-- | Merge the first duration from one comparsion with the second duration from
-- another comparison.
mergeComparisons :: Comparison -> Comparison -> Comparison
mergeComparisons c1 c2 =
  Comparison (comparisonDuration1 c1) (comparisonDuration2 c2)

-- | A comparison succceds if both values exist and the first is greater or
-- equal than the second.
compareDurations :: (Double -> Double -> Bool) -> Comparison -> Bool
compareDurations prop (Comparison (Just d1) (Just d2)) = prop d1 d2
compareDurations _ _ = False

-- * Criterion

-- | Dataype representing a row of results from Criterion.
data Result = Result
  { name     :: !String
  , mean     :: !Double
  , meanLB   :: !Double
  , meanUB   :: !Double
  , stddev   :: !Double
  , stddevLB :: !Double
  , stddevUB :: !Double
  }

-- | Instance to parse a result from a named CSV row.
instance FromNamedRecord Result where
  parseNamedRecord r = Result <$> r .: "Name"
                              <*> r .: "Mean"
                              <*> r .: "MeanLB"
                              <*> r .: "MeanUB"
                              <*> r .: "Stddev"
                              <*> r .: "StddevLB"
                              <*> r .: "StddevUB"

-- | Build a map of comparisons from a vector of results read from the CSV
-- file. We use this auxiliary type so we can use Map union to merge results
-- from two files.
resultsToMapWith :: (Double -> Comparison)
                 -> V.Vector Result
                 -> M.Map String Comparison
resultsToMapWith f = vectorToMap . V.map (\r -> (name r, f (mean r)))

-- * Auxiliary

-- | Turn a vector into a map
vectorToMap :: Ord key
            => V.Vector (key, value)
            -> M.Map key value
vectorToMap vec =
  V.foldl (\myMap (key, value) -> M.insert key value myMap) M.empty vec
