-- |
-- Description : A benchmark for Yampa.
-- Copyright   : (c) Ivan Perez, 2023
-- Authors     : Ivan Perez
--
-- A benchmark for Yampa.
module Main where

import Criterion           (bench, bgroup, nf)
import Criterion.Main      (defaultConfig, defaultMainWith)
import Criterion.Types     (Config(csvFile, resamples, verbosity)
                           , Verbosity(Quiet))
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format    (formatTime, defaultTimeLocale)
import System.Environment  (getArgs, withArgs)
import System.FilePath     ((</>))

import FRP.Yampa

-- | Run all benchmarks.
main :: IO ()
main = do
  config <- customConfig
  withArgs [] $
    defaultMainWith config
       [ bgroup "basic"
                [ bench "identity" $ nf basicIdentity 10000
                , bench "id"       $ nf basicId       10000
                ]
       , bgroup "compositions"
                [ bench "identity" $ nf composeIdentity 10000
                , bench "idid"     $ nf composeIdId     10000
                , bench "plus"     $ nf composePlus     10000
                , bench "plusplus" $ nf composePlusPlus 10000
                , bench "plusmult" $ nf composePlusMult 10000
                , bench "mult"     $ nf composeMult     10000
                , bench "multmult" $ nf composeMultMult 10000
                ]
       , bgroup "counter"
                [ bench "counter1" $ nf counter1 10000
                , bench "counter2" $ nf counter2 10000
                ]
       ]

-- * Benchmarks

-- ** Basic

-- | Yampa's specialized identity function.
basicIdentity :: Int -> [Int]
basicIdentity n = embed sf stream
  where
    sf     = identity
    stream = deltaEncode 1.0 (replicate n 1)

-- | Standard function identity lifted to SFs.
basicId :: Int -> [Int]
basicId n = embed sf stream
  where
    sf     = arr id
    stream = deltaEncode 1.0 (replicate n 1)

-- ** Compositions

-- | Composition of Yampa's specialized identity function.
composeIdentity :: Int -> [Int]
composeIdentity n = embed sf stream
  where
    sf     = identity >>> identity
    stream = deltaEncode 1.0 (replicate n 1)

-- | Composition of standard function identity lifted to SFs.
composeIdId :: Int -> [Int]
composeIdId n = embed sf stream
  where
    sf     = arr id >>> arr id
    stream = deltaEncode 1.0 (replicate n 1)

-- | Plus operation.
--
-- This is not a composition; it merely exists to serve as a comparison with
-- composePlusPlus.
composePlus :: Int -> [Int]
composePlus n = embed sf stream
  where
    sf     = arr (+3)
    stream = deltaEncode 1.0 $ take n [1..]

-- | Composition of addition lifted to SFs.
composePlusPlus :: Int -> [Int]
composePlusPlus n = embed sf stream
  where
    sf     = arr (+1) >>> arr (+2)
    stream = deltaEncode 1.0 $ take n [1..]

-- | Composition of addition with multiplication, lifted to SFs.
composePlusMult :: Int -> [Int]
composePlusMult n = embed sf stream
  where
    sf     = arr (+100) >>> arr (*2)
    stream = deltaEncode 1.0 $ take n [10..]

-- | Multiplication operation.
--
-- This is not a composition; it merely exists to serve as a comparison with
-- composeMultMult.
composeMult :: Int -> [Int]
composeMult n = embed sf stream
  where
    sf     = arr (*20)
    stream = deltaEncode 1.0 $ take n [10..]

-- | Composition of multiplication lifted to SFs.
composeMultMult :: Int -> [Int]
composeMultMult n = embed sf stream
  where
    sf     = arr (*10) >>> arr (*2)
    stream = deltaEncode 1.0 $ take n [10..]

-- ** Counter

-- | Counter without explicit seq.
counter1 :: Int -> [Int]
counter1 n = embed sf stream
  where
    sf     = loopPre 0 (arr (dup . uncurry (+)))
    stream = deltaEncode 1.0 (replicate n 1)

-- | Counter with explicit seq.
counter2 :: Int -> [Int]
counter2 n = embed sf stream
  where
    sf     = loopPre 0 (arr ((\x -> x `seq` (x, x)). uncurry (+)))
    stream = deltaEncode 1.0 (replicate n 1)

-- * Auxiliary functions

-- Construct a config with increased number of sampling
-- and a custom name for the report.
customConfig :: IO Config
customConfig = do
  args <- getArgs

  let dir = case args of
              []     -> "."
              (x:xs) -> x

  -- Custom filename using the current time
  timeString <- (formatTime defaultTimeLocale "%F-%H%M%S") <$> getZonedTime
  let filename = concat [ timeString, "-", "bench.csv" ]

  return $ defaultConfig { csvFile   = Just $ dir </> filename
                         , resamples = 100000
                         , verbosity = Quiet
                         }
