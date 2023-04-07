{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright  : (c) Ivan Perez, 2017-2022
-- License    : BSD-style (see the LICENSE file in the distribution)
-- Maintainer : ivan.perez@keera.co.uk
--
-- QuickCheck generators for input streams.
--
-- Random stream generation can be customized usin three parameters:
--
-- - The distribution for the random time deltas ('Distribution').
-- - The maximum and minimum bounds for the time deltas ('Range').
-- - The maximum stream length ('Length').
--
-- The main function to generate streams is 'generateStream'. The specific time
-- deltas can be customized further using 'generateStreamWith'. Some helper
-- functions are provided to facilitate testing.
module FRP.Yampa.QuickCheck
    (
      -- * Random stream generation
      generateStream
    , generateStreamWith

      -- ** Parameters used to generate random input streams
    , Distribution(..)
    , Range
    , Length

      -- ** Helpers for common cases
    , uniDistStream
    , uniDistStreamMaxDT
    , fixedDelayStream
    , fixedDelayStreamWith
    )
  where

-- External imports
import Control.Applicative (pure, (<$>))
import Data.Random.Normal  (normal')
import FRP.Yampa           (DTime)
import Test.QuickCheck     (Arbitrary (arbitrary), choose, getPositive,
                            suchThat)
import Test.QuickCheck.Gen (Gen (MkGen))

-- Internal imports
import FRP.Yampa.Stream (SignalSampleStream, groupDeltas)

-- * Random stream generation

-- | Generate random stream.
generateStream :: Arbitrary a
               => Distribution -> Range -> Length -> Gen (SignalSampleStream a)
generateStream = generateStreamWith (\_ _ -> arbitrary)

-- | Generate random stream, parameterized by the value generator.
generateStreamWith :: Arbitrary a
                   => (Int -> DTime -> Gen a)
                   -> Distribution
                   -> Range
                   -> Length
                   -> Gen (SignalSampleStream a)
generateStreamWith arb DistConstant range len =
  generateConstantStream arb =<< generateStreamLenDT range len
generateStreamWith arb dist (m, n) len = do
    ds <- generateDeltas len
    let l = length ds
    let f n = arb n (ds !! (n - 1))
    xs <- vectorOfWith l f

    x <- arb 0 0
    return $ groupDeltas (x:xs) ds

  where

    deltaF :: Gen DTime
    deltaF = case dist of
               DistRandom -> generateDelta m n
               DistNormal (avg, stddev) -> generateDSNormal avg stddev m n
               _ -> error "yampa-test: generateStreamWith"

    generateDeltas :: Length -> Gen [DTime]
    generateDeltas Nothing              = do l <- arbitrary
                                             vectorOfWith l (\_ -> deltaF)
    generateDeltas (Just (Left l))      = vectorOfWith l (\_ -> deltaF)
    generateDeltas (Just (Right maxds)) = timeStampsUntilWith deltaF maxds

-- | Generate arbitrary stream with fixed length and constant delta.
generateConstantStream :: (Int -> DTime -> Gen a)
                       -> (DTime, Int)
                       -> Gen (SignalSampleStream a)
generateConstantStream arb (x, length) = do
    ys <- vectorOfWith length (\n -> arb n x)
    return $ groupDeltas ys ds
  where
    ds = repeat x

-- | Generate arbitrary stream
generateStreamLenDT :: (Maybe DTime, Maybe DTime)
                    -> Maybe (Either Int DTime)
                    -> Gen (DTime, Int)
generateStreamLenDT range len = do
  x <- uncurry generateDelta range
  l <- case len of
         Nothing         -> (1 +) . getPositive <$> arbitrary
         Just (Left l)   -> pure l
         Just (Right ds) -> max 1 <$> pure (floor (ds / x))
  return (x, l)

-- | Generate one random delta, possibly within a range.
generateDelta :: Maybe DTime -> Maybe DTime -> Gen DTime
generateDelta (Just x)  (Just y) = choose (x, y)
generateDelta (Just x)  Nothing  = (x +) <$> arbitrary
generateDelta Nothing   (Just y) = choose (2.2251e-308, y)
generateDelta Nothing   Nothing  = getPositive <$> arbitrary

-- | Generate a random delta following a normal distribution, and possibly
-- within a given range.
generateDSNormal :: DTime -> DTime -> Maybe DTime -> Maybe DTime -> Gen DTime
generateDSNormal avg stddev m n = suchThat gen (\x -> mx x && mn x)
  where
    gen = MkGen (\r _ -> fst $ normal' (avg, stddev) r)
    mn  = maybe (\_ -> True) (<=) m
    mx  = maybe (\_ -> True) (>=) n

-- | Generate random samples up until a max time, with a given time delta
-- generation function.
timeStampsUntilWith :: Gen DTime -> DTime -> Gen [DTime]
timeStampsUntilWith arb ds = timeStampsUntilWith' arb [] ds
  where
    -- | Generate random samples up until a max time, with a given time delta
    --   generation function, and an initial suffix of time deltas.
    timeStampsUntilWith' :: Gen DTime -> [DTime] -> DTime -> Gen [DTime]
    timeStampsUntilWith' arb acc ds
      | ds < 0    = return acc
      | otherwise = do d <- arb
                       let acc' = acc `seq` (d:acc)
                       acc' `seq` timeStampsUntilWith' arb acc' (ds - d)

-- ** Parameters used to generate random input streams

-- | Distributions used for time delta (DT) generation.
data Distribution
  = DistConstant                -- ^ Constant DT for the whole stream.
  | DistNormal (DTime, DTime)   -- ^ Variable DT following normal distribution,
                                --   with an average and a standard deviation.
  | DistRandom                  -- ^ Completely random (positive) DT.

-- | Upper and lower bounds of time deltas for random DT generation.
type Range = (Maybe DTime, Maybe DTime)

-- | Optional maximum length for a stream, given as a time, or a number of
-- samples.
type Length = Maybe (Either Int DTime)

-- ** Helpers for common cases

-- | Generate a stream of values with uniformly distributed time deltas.
uniDistStream :: Arbitrary a => Gen (SignalSampleStream a)
uniDistStream = generateStream DistRandom (Nothing, Nothing) Nothing

-- | Generate a stream of values with uniformly distributed time deltas, with a
-- max DT.
uniDistStreamMaxDT :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
uniDistStreamMaxDT maxDT =
  generateStream DistRandom (Nothing, Just maxDT ) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStream :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
fixedDelayStream dt = generateStream DistConstant (Just dt, Just dt) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStreamWith :: Arbitrary a
                     => (DTime -> a)
                     ->  DTime
                     -> Gen (SignalSampleStream a)
fixedDelayStreamWith f dt =
    generateStreamWith f' DistConstant (Just dt, Just dt) Nothing
  where
    f' n t = return $ f (fromIntegral n * t)

-- * Extended quickcheck generator

-- | Generates a list of the given length.
vectorOfWith :: Int -> (Int -> Gen a) -> Gen [a]
vectorOfWith k genF = sequence [ genF i | i <- [1..k] ]
