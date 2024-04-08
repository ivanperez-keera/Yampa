{-# LANGUAGE CPP #-}
-- |
-- Description : Test cases for basic signal functions
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson, Ivan Perez
module Test.FRP.Yampa.Basic
    ( tests
    )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<*>))
import Data.Functor        ((<$>))
#endif
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Basic"
  [ testProperty "identity (fixed)"  (property $ basicsf_t0 ~= basicsf_t0r)
  , testProperty "identity (qc)"     prop_basic_identity_1
  , testProperty "identity (qc)"     prop_basic_identity_2
  , testProperty "constant (fixed)"  (property $ basicsf_t1 ~= basicsf_t1r)
  , testProperty "constant (qc)"     prop_basic_constant
  , testProperty "--> (qc)"          propInsert
  , testProperty "-:> (qc)"          propAlterFirstOutput
  , testProperty ">-- (qc)"          propInputInit
  , testProperty "-=> (qc)"          propModFirstOutput
  , testProperty ">=- (qc)"          propModFirstInput
  , testProperty "initially (fixed)" (property $ basicsf_t4 ~= basicsf_t4r)
  , testProperty "initially (qc)"    prop_basic_initially
  ]

-- * Basic signal functions

-- ** identity

basicsf_t0 :: [Double]
basicsf_t0 = testSF1 identity
basicsf_t0r =
  [ 0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

-- Yampa's Basic SF builders
prop_basic_identity_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream
    sf   = identity
    pred = (==)

prop_basic_identity_2 =
    forAll myStream (evalT $ prop_always_equal identity (arr id))
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

-- ** constant

basicsf_t1 :: [Double]
basicsf_t1 = testSF1 (constant 42.0)
basicsf_t1r =
  [ 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0
  ]

prop_basic_constant =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    sf   = constant 42.0
    pred = const (== 42.0)

-- * Initialization

-- ** @(-->)@

-- | Test that @initialValue --> integral@, when applied to any signal, is
-- initially equal to @constant initialValue@, and, in the future, always equal
-- to @integral@.
--
-- Note that it is important to understand that integral is "turned on" at time
-- zero, and its value discarded. This is not the same as using the constant SF
-- at time zero and turning @integral@ on at the next time (e.g., with
-- @switch@).
propInsert :: Property
propInsert =
    forAll initialValueG $ \initialValue ->
    forAll myStream $ evalT $

      -- SF that uses the actual function being tested
      let sfStep :: SF Float Float
          sfStep = initialValue --> integral

      -- Expected behavior
      in And
           -- Currently equal to initialValue
           (SP $ (==) <$> sfStep <*> constant initialValue)

           -- In the future, always equal to integral
           (Next $ Always $ SP $ (==) <$> sfStep <*> integral)

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

-- ** @(-:>)@

-- | Test that @initialValue -:> sf@, when applied to any signal, is initially
-- equal to @constant initialValue@, and, after that, the output is the same as
-- @sf@ (starting from that point).
propAlterFirstOutput :: Property
propAlterFirstOutput =
    forAll initialValueG $ \initialValue ->
    forAll myStream $ evalT $

      -- SF that uses the actual function being tested.
      --
      -- We pick a point-wise function for the test because we have no (easy)
      -- way of dropping a sample when we "turn of" the second sf in the
      -- comparison for future samples. For example, if we had picked integral,
      -- then the integral would start from the second sample (with the delay
      -- applied), but the temporal Next operator starts the SF now and waits
      -- until the next sample to check the property.
      let sfStep :: SF Float Float
          sfStep = (initialValue -:> arr (* 2)) >>> arr (^ 2)

      -- Expected behavior
      in And
           -- Currently equal to initialValue. In tis case it is safe to
           -- compare floating point numbers with (==) because the output HAS
           -- to be exactly the same. Note that, in the efinition of sfStep,
           -- the number initialValue should reach (arr (^ 2)) unchanged, and
           -- arr is just function application.
           (SP $ (==) <$> sfStep <*> constant (initialValue ^ 2))

           -- In the future, always equal to (arr ((^ 2) . (* 2)))). Note that
           -- we ignore initialValue completely.
           (Next $ Always $ SP $ (==) <$> sfStep <*> arr ((^ 2) . (* 2)))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

-- ** @(>--)@

-- | Test that @initialValue >-- sf@, when applied to any signal, is initially
-- equal to @initialValue@, and, in the future, always equal to @sf@.
propInputInit :: Property
propInputInit =
    forAll initialValueG $ \initialValue ->
    forAll myStream $ evalT $

      -- SF that uses the actual function being tested
      let sfStep :: SF Float Float
          sfStep = initialValue --> arr (* 2)

      -- Expected behavior
      in And
           -- Currently equal to initialValue
           (SP $ (==) <$> sfStep <*> constant initialValue)

           -- In the future, always equal to arr (* 2)
           (Next $ Always $ SP $ (==) <$> sfStep <*> arr (* 2))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

-- ** (-=>)

-- | Test that @(-=>)@ applies a transformation to the first output.
--
-- We test with the specific function @(* 4) -=> arr (^ 2)@.
propModFirstOutput :: Property
propModFirstOutput =
    forAll myStream $ evalT $

      -- SF that uses the actual function being tested.
      --
      -- We specifically pick transformations that do not commute, so that we
      -- test that transformations are applied in the expected order.
      let sfStep :: SF Float Float
          sfStep = (* 4) -=> arr (^ 2)

      -- Expected behavior
      in And
           -- Initially, both transformations are applied. Note that the
           -- difference between this comparison and the one for (>=-) is the
           -- order in which the two transformations are applied.
           (SP $ (==) <$> sfStep <*> arr ((* 4) . (^ 2)))

           -- In the future, only the second transformation is applied
           (Next $ Always $ SP $ (==) <$> sfStep <*> arr (^ 2))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

-- ** @(>=-)@

-- | Test that @f -=> arr (^ 2)@, when applied to any signal, is initially
-- equal to initialValue, and, in the future, always equal to @arr (^ 2)@.
propModFirstInput :: Property
propModFirstInput =
    forAll myStream $ evalT $

      -- SF that uses the actual function being tested.
      --
      -- We specifically pick transformations that do not commute, so that we
      -- test that transformations are applied in the expected order.
      let sfStep :: SF Float Float
          sfStep = (* 2) >=- arr (^ 2)

      -- Expected behavior
      in And
           -- Initially, both transformations are applied. Note that the
           -- difference between this comparison and the one for (-=>) is the
           -- order in which the two transformations are applied.
           (SP $ (==) <$> sfStep <*> arr ((^ 2) . (* 2)))

           -- In the future, only the second transformation is applied
           (Next $ Always $ SP $ (==) <$> sfStep <*> arr (^ 2))

  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    initialValueG :: Gen Float
    initialValueG = arbitrary

-- ** initially

basicsf_t4 :: [Double]
basicsf_t4 = testSF1 (initially 42.0)
basicsf_t4r =
  [ 42.0, 1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

prop_basic_initially =
    forAll myStream $ evalT $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    sf   = initially 42.0
    pred = const (== 42.0)

-- * Auxiliary

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

-- | Compares two SFs, resulting in true if they are always equal
prop_always_equal sf1 sf2 =
    Always $ SP ((sf1 &&& sf2) >>> arr sameResult)
  where
    sameResult = uncurry (==)
