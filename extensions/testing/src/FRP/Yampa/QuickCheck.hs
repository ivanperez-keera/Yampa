{-# LANGUAGE Arrows              #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | QuickCheck generators for input streams.
--
-- Random stream generation can be customized usin three parameters:
--
-- - The distribution for the random time deltas ('Distribution').
-- - The maximum and minimum bounds for the time deltas ('Range').
-- - The maximum stream length ('Length').
--
-- The main function to generate streams is 'generateStream'. The specific
-- time deltas can be customized further using 'generateStreamWith'. Some
-- helper functions are provided to facilitate testing.

-- The function uniDistStreamMaxDT had the wrong type and the name on the
-- paper was: uniDistStream. This has been fixed.

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

import Control.Applicative ((<$>), pure)
import Data.Random.Normal
import FRP.Yampa
import Test.QuickCheck
import Test.QuickCheck.Gen

import FRP.Yampa.Stream

-- | Distributions used for time delta (DT) generation.
data Distribution = DistConstant                -- ^ Constant DT for the whole stream.
                  | DistNormal (DTime, DTime)   -- ^ Variable DT following normal distribution,
                                                --   with an average and a standard deviation.
                  | DistRandom                  -- ^ Completely random (positive) DT.

-- | Upper and lower bounds of time deltas for random DT generation.
type Range = (Maybe DTime, Maybe DTime)

-- | Optional maximum length for a stream, given as a time, or a number of
-- samples.
type Length = Maybe (Either Int DTime)


-- | Generate a random delta according to some required specifications.
generateDeltas :: Distribution -> Range -> Length -> Gen DTime
generateDeltas DistConstant            (mn, mx) len = generateDelta mn mx
generateDeltas DistRandom              (mn, mx) len = generateDelta mn mx
generateDeltas (DistNormal (avg, dev)) (mn, mx) len = generateDSNormal avg dev mn mx

-- | Generate one random delta, possibly within a range.
generateDelta :: Maybe DTime -> Maybe DTime -> Gen DTime
generateDelta (Just x)  (Just y)  = choose (x, y)
generateDelta (Just x)  (Nothing) = (x+) <$> arbitrary
generateDelta (Nothing) (Just y)  = choose (2.2251e-308, y)
generateDelta (Nothing) (Nothing) = getPositive <$> arbitrary

-- | Generate a random delta following a normal distribution,
--   and possibly within a given range.
generateDSNormal :: DTime -> DTime -> Maybe DTime -> Maybe DTime -> Gen DTime
generateDSNormal avg stddev m n = suchThat gen (\x -> mx x && mn x)
  where
    gen = MkGen (\r _ -> let (x,_) = normal' (avg, stddev) r in x)
    mn  = maybe (\_ -> True) (<=) m
    mx  = maybe (\_ -> True) (>=) n

-- | Generate random samples up until a max time.
timeStampsUntil :: DTime -> Gen [DTime]
timeStampsUntil = timeStampsUntilWith arbitrary

-- | Generate random samples up until a max time, with a given time delta
--   generation function.
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

-- | Generate random stream.
generateStream :: Arbitrary a
               => Distribution -> Range -> Length -> Gen (SignalSampleStream a)
generateStream = generateStreamWith (\_ _ -> arbitrary)

-- | Generate random stream, parameterized by the value generator.
generateStreamWith :: Arbitrary a
                   => (Int -> DTime -> Gen a) -> Distribution -> Range -> Length -> Gen (SignalSampleStream a)
generateStreamWith arb DistConstant range  len     = generateConstantStream arb =<< generateStreamLenDT range len
generateStreamWith arb DistRandom   (m, n) Nothing = do
  l <- arbitrary
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDelta m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb DistRandom (m, n) (Just (Left l)) = do
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDelta m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb DistRandom (m, n) (Just (Right maxds)) = do
  ds <- timeStampsUntilWith (generateDelta m n) maxds
  let l = length ds
  x  <- arb 0 0
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) Nothing = do
  l <- arbitrary
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDSNormal avg stddev m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) (Just (Left l)) = do
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDSNormal avg stddev m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) (Just (Right maxds)) = do
  ds <- timeStampsUntilWith (generateDSNormal avg stddev m n) maxds
  let l = length ds
  x <- arb 0 0
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

-- | Generate arbitrary stream with fixed length and constant delta.
generateConstantStream :: (Int -> DTime -> Gen a) -> (DTime, Int) -> Gen (SignalSampleStream a)
generateConstantStream arb (x, length) = do
  ys <- vectorOfWith length (\n -> arb n x)
  let ds = repeat x
  return $ groupDeltas ys ds

-- | Generate arbitrary stream
generateStreamLenDT :: (Maybe DTime, Maybe DTime) -> Maybe (Either Int DTime) -> Gen (DTime, Int)
generateStreamLenDT range len = do
  x <- uncurry generateDelta range
  l <- case len of
         Nothing         -> ((1 +) . getPositive) <$> arbitrary
         Just (Left l)   -> pure l
         Just (Right ds) -> (max 1) <$> (pure (floor (ds / x)))
  return (x, l)

-- generateStreamLenDT (Just x,  Just y)  (Just (Left l))   = (,) <$> choose (x, y)        <*> pure l
-- generateStreamLenDT (Just x,  Nothing) (Just (Left l))   = (,) <$> ((x+) <$> arbitrary) <*> pure l
-- generateStreamLenDT (Nothing, Just y)  (Just (Left l))   = (,) <$> choose (0, y)        <*> pure l
-- generateStreamLenDT (Just x,  _)       (Just (Right ts)) = (,) <$> pure x               <*> pure (floor (ts / x))
-- generateStreamLenDT (Just x,  _)       Nothing           = (,) <$> pure x               <*> arbitrary
-- generateStreamLenDT (Nothing, Nothing) Nothing           = (,) <$> arbitrary            <*> arbitrary
-- generateStreamLenDT (Nothing, Nothing) (Just (Left l))   = (,) <$> arbitrary            <*> pure l
-- generateStreamLenDT (Nothing, Nothing) (Just (Right ds)) = f2  <$> arbitrary
--   where
--     f2 l = (ds / fromIntegral l, l)


-- | Generate a stream of values with uniformly distributed time deltas.
uniDistStream :: Arbitrary a => Gen (SignalSampleStream a)
uniDistStream = generateStream DistRandom (Nothing, Nothing) Nothing

-- | Generate a stream of values with uniformly distributed time deltas, with a max DT.
uniDistStreamMaxDT :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
uniDistStreamMaxDT maxDT = generateStream DistRandom (Nothing, Just maxDT ) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStream :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
fixedDelayStream dt = generateStream DistConstant (Just dt, Just dt) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStreamWith :: Arbitrary a => (DTime -> a) ->  DTime -> Gen (SignalSampleStream a)
fixedDelayStreamWith f dt = generateStreamWith f' DistConstant (Just dt, Just dt) Nothing
  where
    f' n t = return $ f (fromIntegral n * t)

-- * Extended quickcheck generator

-- | Generates a list of the given length.
vectorOfWith :: Int -> (Int -> Gen a) -> Gen [a]
vectorOfWith k genF = sequence [ genF i | i <- [1..k] ]
