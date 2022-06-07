-- |
-- Description : Test cases for basic signal functions
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Basic
    ( tests
    )
  where

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
  , testProperty "initially (fixed)" (property $ basicsf_t4 ~= basicsf_t4r)
  , testProperty "initially (qc)"    prop_basic_initially
  ]

-- * Basic signal functions

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
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        sf   = identity
        pred = (==)

prop_basic_identity_2 =
    forAll myStream (evalT $ prop_always_equal identity (arr id))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

basicsf_t1 :: [Double]
basicsf_t1 = testSF1 (constant 42.0)
basicsf_t1r =
  [ 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0
  ]

prop_basic_constant =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = constant 42.0
        pred = const (== 42.0)

-- * Initialization

prop_insert =
    forAll initialValueG $ \initialValue ->
    forAll finalValueG $ \finalValue ->
    forAll myStream $ evalT $
      let sfStep = initialValue --> constant finalValue

      in And (prop (sfStep, const (== initialValue)))
             (Next $ Always $
                       (prop (sfStep, const (== finalValue))))

  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        initialValueG :: Gen Float
        initialValueG = arbitrary

        finalValueG  :: Gen Float
        finalValueG = arbitrary

basicsf_t4 :: [Double]
basicsf_t4 = testSF1 (initially 42.0)
basicsf_t4r =
  [ 42.0, 1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

prop_basic_initially =
    forAll myStream $ evalT $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = initially 42.0
        pred = const (== 42.0)

-- * Auxiliary

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

-- | Compares two SFs, resulting in true if they are always equal
prop_always_equal sf1 sf2 =
    Always $ SP ((sf1 &&& sf2) >>> arr sameResult)
  where sameResult = uncurry (==)
