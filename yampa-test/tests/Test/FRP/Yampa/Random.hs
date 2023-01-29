{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Description : Test cases for signal functions working with random values.
-- Copyright   : (c) Ivan Perez, 2023
-- Authors     : Ivan Perez

module Test.FRP.Yampa.Random
    ( tests
    )
  where

#if __GLASGOW_HASKELL__ < 708
import Data.Bits (bitSize)
#endif
#if __GLASGOW_HASKELL__ >= 708
import Data.Bits (bitSizeMaybe)
#endif

import Data.Bits             (Bits, popCount)
import Data.Maybe            (fromMaybe)
import Data.Word             (Word32, Word64)
import Foreign.C             (CFloat(..))
import System.Random         (mkStdGen)
import Test.QuickCheck       hiding (once, sample)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa            (embed, noise, noiseR, second)
import FRP.Yampa.QuickCheck (Distribution (DistRandom), generateStream)
import FRP.Yampa.Stream     (SignalSampleStream)

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Random"
  [ testProperty "noise (0, qc)"  propNoise
  , testProperty "noiseR (0, qc)" propNoiseR
  ]

-- * Noise (i.e. random signal generators) and stochastic processes

propNoise :: Property
propNoise =
    forAll genSeed $ \seed ->
    forAll myStream $ \stream ->
      isRandom (embed (noise (mkStdGen seed)) (structure stream) :: [Word32])
  where
    -- Generator: Input stream.
    --
    -- We provide a number of samples; otherwise, deviations might not indicate
    -- lack of randomness for the signal function.
    myStream :: Gen (SignalSampleStream ())
    myStream =
      generateStream DistRandom (Nothing, Nothing) (Just (Left numSamples))

    -- Generator: Random generator seed
    genSeed :: Gen Int
    genSeed = arbitrary

    -- Constant: Number of samples in the stream used for testing.
    --
    -- This number has to be high; numbers 100 or below will likely not work.
    numSamples :: Int
    numSamples = 400

propNoiseR :: Property
propNoiseR =
    forAll genSeed $ \seed ->
    forAll myStream $ \stream ->
      -- True if the noise signal is within the given bounds, and it is random
      -- when constrained to that range.
      let output = embed (noiseR bounds (mkStdGen seed)) (structure stream)
      in all (`isInRange` bounds) output && isRandom (constrainTypes output)

  where

    -- Generator: Input stream.
    --
    -- We provide a number of samples; otherwise, deviations might not indicate
    -- lack of randomness for the signal function.
    myStream :: Gen (SignalSampleStream ())
    myStream =
      generateStream DistRandom (Nothing, Nothing) (Just (Left numSamples))

    -- Generator: Random generator seed
    genSeed :: Gen Int
    genSeed = arbitrary

    -- Constant: Bounds used for the test.
    --
    -- We bound the numbers generated to the 32-bit range, but express it
    -- using the type of Word64.
    bounds :: (Word64, Word64)
    bounds = (min32, max32)
      where
        min32 = fromIntegral (minBound :: Word32)
        max32 = fromIntegral (maxBound :: Word32)

    -- Constant: Number of samples in the stream used for testing.
    --
    -- This number has to be high; numbers 100 or below will likely not work.
    numSamples :: Int
    numSamples = 400

    -- Constrain the types of the argument list to the output type.
    --
    -- For this test to work, this type must be consistent with the bounds
    -- chosen in the constant 'bounds'.
    constrainTypes :: [Word64] -> [Word32]
    constrainTypes = map fromIntegral

    -- | True if the argument is within the given range, false otherwise.
    isInRange :: Ord a => a -> (a, a) -> Bool
    isInRange x (minB, maxB) = minB <= x && x <= maxB

-- * Auxiliary definitions

-- | Check whether a list of values exhibits randomness.
--
-- This function implements the Frequence (Monobit) Test, as described in
-- Section 2.1 of "A Statistical Test Suite for Random and Pseudorandom Number
-- Generators for Cryptographic Applications", by Rukhin et al.
isRandom :: Bits a => [a] -> Bool
isRandom ls = pValue >= 0.01
  where
    pValue = erfc (sObs / sqrt 2)
    sObs   = abs sn / sqrt n
    n      = fromIntegral $ elemSize * length ls
    sn     = sum $ map numConv ls

    -- Number of bits per element
    elemSize :: Int
    elemSize =
      -- bitSize' ignores the argument, so it's ok if the list is empty
      bitSize' $ head ls

    -- Substitute each digit e in the binary representation of the input value
    -- by 2e – 1, and add the results.
    numConv :: Bits a => a -> Float
    numConv x = fromIntegral $ numOnes - numZeroes
      where
        numOnes   = popCount x
        numZeroes = elemSize - popCount x

        -- Number of bits per element
        elemSize = bitSize' x

-- | Complementary Error Function, compliant with the definition of erfcf in
-- ANSI C.
erfc :: Float -> Float
erfc = realToFrac . erfcf . realToFrac

-- | ANSI C function erfcf defined in math.h
foreign import ccall "erfcf" erfcf :: CFloat -> CFloat

-- | Transform SignalSampleStreams into streams of differences.
structure :: (a, [(b, a)]) -> (a, [(b, Maybe a)])
structure (x, xs) = (x, map (second Just) xs)

-- | Implementation of bitSize that uses bitSize/bitSizeMaybe depending on the
-- version of base available.
bitSize' :: Bits a => a -> Int
bitSize' =
#if __GLASGOW_HASKELL__ < 708
  bitSize
#else
  fromMaybe 0 . bitSizeMaybe
#endif
