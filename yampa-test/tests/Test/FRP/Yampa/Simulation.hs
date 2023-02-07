{-# LANGUAGE CPP #-}
-- |
-- Description : Test cases for FRP.Yampa.Simulation
-- Copyright   : (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson
module Test.FRP.Yampa.Simulation
    ( tests
    )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<*>))
import Data.Functor        ((<$>))
#endif

import Data.Maybe       (fromMaybe)
import Data.Traversable (mapAccumL)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)

import FRP.Yampa as Yampa

import FRP.Yampa.QuickCheck (uniDistStream)
import FRP.Yampa.Stream     (FutureSampleStream, SignalSampleStream)

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Simulation"
  [ testProperty "reactimate (fixed)"    (property $ react_t0 ~= react_t0r)
  , testProperty "react, reactInit (qc)" testReact
  , testProperty "embed (0, qc)"         testEmbed
  , testProperty "embedSynch (0, fixed)" (property $ embed_t0 ~= embed_t0r)
  , testProperty "embedSynch (1, fixed)" (property $ embed_t1 ~= embed_t1r)
  , testProperty "deltaEncode (0, qc)"   testDeltaEncode
  , testProperty "deltaEncodeBy (0, qc)" testDeltaEncodeBy
  , testProperty "evalAtZero (0, qc)"    testEvalAtZero
  , testProperty "evalAt (0, qc)"        testEvalAt
  , testProperty "evalFuture (0, qc)"    testEvalFuture
  ]

-- * Reactimation

react_t0 :: [(Double, Double)]
react_t0 = unsafePerformIO $ do
  countr   <- newIORef undefined
  inputr   <- newIORef undefined
  outputsr <- newIORef []
  let init = do
        writeIORef countr 1
        let input0 = 0.0
        writeIORef inputr input0
        return input0
      sense _ = do
        count <- readIORef countr
        if count >= 5
          then do
            writeIORef countr 1
            input <- readIORef inputr
            let input' = input + 0.5
            writeIORef inputr input'
            return (0.1, Just input')
          else do
            writeIORef countr (count + 1)
            return (0.1, Nothing)
      actuate _ output = do
        outputs <- readIORef outputsr
        writeIORef outputsr (output : outputs)
        input <- readIORef inputr
        return (input > 5.0)
  reactimate init sense actuate (arr dup >>> second integral)
  outputs <- readIORef outputsr
  return (take 25 (reverse outputs))

react_t0r :: [(Double, Double)]
react_t0r =
  [ (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00)
  , (0.5,0.00), (0.5,0.05), (0.5,0.10), (0.5,0.15), (0.5,0.20)
  , (1.0,0.25), (1.0,0.35), (1.0,0.45), (1.0,0.55), (1.0,0.65)
  , (1.5,0.75), (1.5,0.90), (1.5,1.05), (1.5,1.20), (1.5,1.35)
  , (2.0,1.50), (2.0,1.70), (2.0,1.90), (2.0,2.10), (2.0,2.30)
  ]

-- ** Low-level reactimation interface

testReact :: Property
testReact =
    forAll myStream $ \s ->
    forAllBlind randomSF $ \sf ->
      ioProperty $ do
        outs <- reactEmbed sf s
        let outsE = embed sf (structure s)
        return $ outs == outsE

  where

    myStream :: Gen (SignalSampleStream Integer)
    myStream = uniDistStream

    reactEmbed :: SF a b -> SignalSampleStream a -> IO [b]
    reactEmbed sf s@(s0, ss) = do
        outsRef <- newIORef []

        let init = return s0

            actuate _ _ b = modifyIORef outsRef (++ [b]) >> return False

            -- For each sample, add a Just to the value of the sample make the
            -- input compatible with what 'react' expects, and use 'react' to
            -- run one step of the simulation.
            reactEmbed' :: ReactHandle a b -> FutureSampleStream a -> IO ()
            reactEmbed' rh = mapM_ (react rh . second Just)

        reactHandle <- reactInit init actuate sf
        reactEmbed' reactHandle ss
        readIORef outsRef

    structure :: (a, [(b, a)]) -> (a, [(b, Maybe a)])
    structure (x, xs) = (x, map (second Just) xs)

-- * Embedding

testEmbed :: Property
testEmbed = testEmbedPointwise
       .&&. testEmbedSum

  where

    testEmbedPointwise :: Property
    testEmbedPointwise =
      forAllBlind function $ \f ->
      forAll myStream $ \stream ->
        property $
          embed (arr f) (structure stream) == fmap f (plain stream)

    testEmbedSum :: Property
    testEmbedSum =
      forAll myStream $ \stream ->
        property $
          let left :: [Integer]
              left = embed sf (structure stream)

              sf :: SF Integer Integer
              sf = loopPre 0 (arr (dup . uncurry (+)))

              right :: [Integer]
              right = summation (plain stream)

          in left == right

    myStream :: Gen (SignalSampleStream Integer)
    myStream = uniDistStream

    function :: Gen (Integer -> Integer)
    function = arbitrary

    -- Make each element the sum of all elements up to that point.
    summation :: [Integer] -> [Integer]
    summation =
      -- We add the accumulator to the current value (+), and make that the new
      -- value AND the new accumulator (dup).
       snd . mapAccumL ((dup .) . (+)) 0

    plain :: SignalSampleStream a -> [a]
    plain (x, xs) = x : fmap snd xs

    structure :: (a, [(b, a)]) -> (a, [(b, Maybe a)])
    structure (x, xs) = (x, map (second Just) xs)

embed_ratio :: SF a Double
embed_ratio = switch (constant 1.0 &&& after 5.0 ()) $ \_ ->
              switch (constant 0.0 &&& after 5.0 ()) $ \_ ->
              constant 3.0

embed_sf :: SF a Double
embed_sf = localTime >>> integral

embed_t0 = take 20 $ embed (embed_ratio
                            >>> embedSynch embed_sf
                                           (deltaEncode 0.01 (repeat ())))
                           (deltaEncode 1.0 (repeat ()))

embed_t0r =
  [   0.0000,   0.4851,   1.9701,    4.4850,   7.9800
  ,   7.9800,   7.9800,   7.9800,    7.9800,   7.9800
  ,  24.4650,  49.9500,  84.4350,  127.9200, 180.2151
  , 241.6701, 312.1251, 391.5801, 480.03510, 577.4901
  ]

embed_t1 = take 20 $ embed (embed_ratio
                            >>> embedSynch embed_sf
                                           (deltaEncode 0.5 (replicate 30 ())))
                           (deltaEncode 1.0 (repeat ()))

embed_t1r =
  [   0.00,   0.25,   1.50,   3.75,   7.00
  ,   7.00,   7.00,   7.00,   7.00,   7.00
  ,  22.75,  47.50,  81.25, 101.50, 101.50
  , 101.50, 101.50, 101.50, 101.50, 101.50
  ]

testDeltaEncode :: Property
testDeltaEncode = testDeltaEncodeSamples
             .&&. testDeltaEncodeTimes

  where

    -- True if the samples produced by deltaEncode are not altered
    testDeltaEncodeSamples :: Property
    testDeltaEncodeSamples =
      forAll randomTime $ \t ->
      forAll randomSamples $ \s ->
        property $ s == streamSamples (deltaEncode t s)

    -- True if the times produced by deltaEncode are not altered
    testDeltaEncodeTimes :: Property
    testDeltaEncodeTimes =
      forAll randomTime $ \t ->
      forAll randomSamples $ \s ->
        property $ all (== t) $ streamTimes (deltaEncode t s)

testDeltaEncodeBy :: Property
testDeltaEncodeBy = testDeltaEncodeBySamples
               .&&. testDeltaEncodeByTimes

  where

    -- True if the samples produced by deltaEncodeBy are not altered
    testDeltaEncodeBySamples :: Property
    testDeltaEncodeBySamples =
        forAll randomTime $ \t ->
        forAll randomSamples $ \s ->
        forAllBlind randomPredicate $ \f ->
          -- Compare all samples, pair-wise, between the list encoded (s) and
          -- the resulting signal samples, using the predicate (f).
          property $ simplifyBy f s == streamSamples (deltaEncodeBy f t s)

      where

        -- Simplify a stream by using an equality predicate function.
        --
        -- The function simplifyBy models the behavior of deltaEncodeBy, which
        -- uses the equality function to compare values. Note that
        -- deltaEncodeBy carries the last value forward even if the equality
        -- was successful (i.e., even if the output for that step is Nothing).
        -- Consequently, when the current value is compared with the last value
        -- to determine if the output must be a Nothing or a Just, the current
        -- value is not compared to the last value for which the equality test
        -- failed, but rather, to the very last value.
        --
        -- The behavior of deltaEncodeBy may appear unsual when the equality
        -- predicate function provided is not transitive.
        simplifyBy :: (a -> a -> Bool) -> [a] -> [a]
        simplifyBy f []     = []
        simplifyBy f (x:xs) = x : simplifyBy' f x x xs
          where
            simplifyBy' :: (a -> a -> Bool) -> a -> a -> [a] -> [a]
            simplifyBy' f _acc _cmp [] = []
            simplifyBy' f acc  cmp  (x:xs)
              | f x cmp   = acc : simplifyBy' f acc x xs
              | otherwise = x : simplifyBy' f x x xs

    -- True if the times produced by deltaEncodeBy are not altered
    testDeltaEncodeByTimes :: Property
    testDeltaEncodeByTimes =
      forAll randomTime $ \t ->
      forAll randomSamples $ \s ->
      forAllBlind randomPredicate $ \f ->
        property $ all (== t) $ streamTimes (deltaEncodeBy f t s)

    -- Predicate on two integer arguments
    randomPredicate :: Gen (Integer -> Integer -> Bool)
    randomPredicate = arbitrary

-- * Debugging / Step by step simulation

testEvalAtZero :: Property
testEvalAtZero = testEvalAtZero1
            .&&. testEvalAtZero2

  where

    testEvalAtZero1 :: Property
    testEvalAtZero1 =
      forAllBlind randomSF $ \sf ->
      forAll arbitrary $ \x ->
        fst (evalAtZero sf x) == head (embed sf (x, []))

    testEvalAtZero2 :: Property
    testEvalAtZero2 =
      forAllBlind randomSF2 $ \sf ->
      forAll arbitrary $ \x ->
        fst (evalAtZero sf x) == head (embed sf (x, []))

testEvalAt :: Property
testEvalAt = testEvalAt1
        .&&. testEvalAt2

  where

    testEvalAt1 :: Property
    testEvalAt1 =
      forAllBlind randomSF $ \sf ->
      forAll arbitrary $ \x1 ->
      forAll arbitrary $ \x2 ->
      forAll randomTime $ \t ->
        let
          -- Value (fst) of simulation after one step with evalAtZero, and
          -- another step with evalAt
          eval1Val = fst $ evalAt (snd (evalAtZero sf x1)) t x2

          -- Second sample (!!1) of result of embedding with stream with two
          -- samples
          embed1Val = (embed sf (x1, [(t, Just x2)])) !! 1

        in eval1Val == embed1Val

    testEvalAt2 :: Property
    testEvalAt2 =
      forAllBlind randomSF2 $ \sf ->
      forAll arbitrary $ \x1 ->
      forAll arbitrary $ \x2 ->
      forAll randomTime $ \t ->
        let
          -- Value (fst) of simulation after one step with evalAtZero, and
          -- another step with evalAt
          eval1Val = fst $ evalAt (snd (evalAtZero sf x1)) t x2

          -- Second sample (!!1) of result of embedding with stream with two
          -- samples
          embed1Val = (embed sf (x1, [(t, Just x2)])) !! 1

        in eval1Val == embed1Val

testEvalFuture :: Property
testEvalFuture = testEvalFuture1
            .&&. testEvalFuture2

  where

    testEvalFuture1 :: Property
    testEvalFuture1 =
        forAllBlind randomSF $ \sf ->
        forAll myStream1 $ \s ->
          unroll sf s == embed sf (structure s)
      where
        myStream1 :: Gen (SignalSampleStream Integer)
        myStream1 = uniDistStream

    testEvalFuture2 :: Property
    testEvalFuture2 =
        forAllBlind randomSF2 $ \sf ->
        forAll myStream2 $ \s ->
          unroll sf s == embed sf (structure s)
      where
        myStream2 :: Gen (SignalSampleStream (Integer, Integer))
        myStream2 = uniDistStream

    -- Apply an SF to a stream of inputs manually, using evalAtZero and
    -- evalFuture, and collect the outputs
    unroll :: SF a b -> (a, [(DTime, a)]) -> [b]
    unroll sf (s1, [])          = [ fst $ evalAtZero sf s1 ]
    unroll sf (s1, (dt, s2):ss) = o1 : unroll sf' (s2, ss)
      where
        (o1, sf') = evalFuture sf s1 dt

    structure :: (a, [(b, a)]) -> (a, [(b, Maybe a)])
    structure (x, xs) = (x, map (second Just) xs)

-- * Auxiliary

-- | Generate a random positive time delta.
randomTime :: Gen Double
randomTime = getPositive <$> arbitrary

-- | Generate multiple random integer samples.
randomSamples :: Gen [Integer]
randomSamples = getNonEmpty <$> arbitrary

-- | Generator of random signal functions on integers.
randomSF :: Gen (SF Integer Integer)
randomSF = oneof [ return identity
                 , pointwiseSF
                 , loopPre <$> arbitrary <*> randomSF2
                 ]

-- | Generator of random signal functions on integer pairs.
randomSF2 :: Gen (SF (Integer, Integer) (Integer, Integer))
randomSF2 = oneof [ return identity
                  , pointwiseSF2
                  ]

-- | Generator of random pointwise signal functions on integers.
pointwiseSF :: Gen (SF Integer Integer)
pointwiseSF = arr <$> arbitrary

-- | Generator of random pointwise signal functions on integer pairs.
pointwiseSF2 :: Gen (SF (Integer, Integer) (Integer, Integer))
pointwiseSF2 = arr <$> arbitrary

-- | Extract the samples from an "optimized" stream.
streamSamples :: (a, [(DTime, Maybe a)]) -> [a]
streamSamples (a, as) = a : streamSamples' a (fmap snd as)
  where
    streamSamples' :: a -> [Maybe a] -> [a]
    streamSamples' acc =
      -- We pick one between the accumulator to the current value
      -- if available (fromMaybe), and make that the new value AND the
      -- new accumulator (dup).
      snd . mapAccumL ((dup .) . fromMaybe) acc

-- | Extract the times from an "optimized" stream.
streamTimes :: (a, [(DTime, Maybe a)]) -> [DTime]
streamTimes = map fst . snd
