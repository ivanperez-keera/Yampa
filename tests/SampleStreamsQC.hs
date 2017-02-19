-- TODO
-- Properties in this file have different types.
-- It's important to agree on the representation type.
--
-- It may be a bit hard, because some elements from logic are
-- provided by QC, while others have to be defined by us.
-- For example, connectives like implication and always are
-- provided by us, and forAll is in QuickCheck.
--
-- This makes it hard to combine, becase for this language to be
-- compositional like logic is we need to make everything accept
-- a QuickCheck predicate, which may not be possible or compatible
-- with out goals.
--
module SampleStreamsQC where

------------------------------------------------------------------------------
import Test.QuickCheck

import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import SampleStreams

-- ** Generators
positiveSignalStream (a,as) = all (>0) $ map fst as

instance Arbitrary a => Arbitrary (TimedSample a) where
  arbitrary = do
    x <- arbitrary
    Positive dt <- arbitrary
    return (TimedSample (dt, x))

uniDistStream :: Arbitrary a => Gen (SignalSampleStream a)
uniDistStream = do
  x <- arbitrary
  rest <- uniDistFutureStream
  return (x, rest)

uniDistFutureStream :: Arbitrary a => Gen (FutureSampleStream a)
uniDistFutureStream = listOf arbitrarySample

arbitrarySample :: Arbitrary a => Gen (DTime, a)
arbitrarySample = do
  Positive dt <- arbitrary
  x <- arbitrary
  return (dt, x)

fixedDelayStream :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
fixedDelayStream dt = do
  x <- arbitrary
  rest <- fixedDelayFutureStream dt
  return (x, rest)

fixedDelayFutureStream :: Arbitrary a => DTime -> Gen (FutureSampleStream a)
fixedDelayFutureStream dt = listOf (arbitrarySampleAt dt)

arbitrarySampleAt :: Arbitrary a => DTime -> Gen (DTime, a)
arbitrarySampleAt dt = do
  x <- arbitrary
  return (dt, x)

fixedDelayStreamWith :: (DTime -> a) -> DTime -> Gen (SignalSampleStream a)
fixedDelayStreamWith f dt = do
  rest <- fixedDelayFutureStreamWith f dt
  return (f 0.0, rest)

fixedDelayFutureStreamWith :: (DTime -> a) -> DTime -> Gen (FutureSampleStream a)
fixedDelayFutureStreamWith f dt = listOfWith (sampleWithAt f dt)

sampleWithAt :: (DTime -> a) -> DTime -> Int -> Gen (DTime, a)
sampleWithAt f dt i = do
  return (dt, f (fromIntegral i * dt))

-- | Generates a list of random length. The maximum length depends on the
-- size parameter.
listOfWith :: (Int -> Gen a) -> Gen [a]
listOfWith genF = sized $ \n ->
  do k <- choose (0,n)
     vectorOfWith k genF

-- | Generates a list of the given length.
vectorOfWith :: Int -> (Int -> Gen a) -> Gen [a]
vectorOfWith k genF = sequence [ genF i | i <- [1..k] ]
