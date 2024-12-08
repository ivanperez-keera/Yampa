-- |
-- Description : Test cases for arrow helper functions.
-- Copyright   : (c) Ivan Perez, 2022
-- Authors     : Ivan Perez
module Test.FRP.Yampa.Arrow
    ( tests
    )
  where

-- External modules
import Test.QuickCheck       (Gen, Property, arbitrary, forAll, forAllBlind)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- External modules: Yampa
import FRP.Yampa as Yampa

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Arrow"
  [ testProperty "dup (qc)"  testDup
  , testProperty "arr2 (qc)" testArr2
  , testProperty "arr3 (qc)" testArr3
  , testProperty "arr4 (qc)" testArr4
  , testProperty "arr5 (qc)" testArr5
  ]

-- * Arrow plumbing aids

testDup :: Property
testDup =
    forAll input $ \x ->
      (fst (dup x) == x) && (snd (dup x) == x)
  where
    input :: Gen Integer
    input = arbitrary

-- * Liftings

testArr2 :: Property
testArr2 =
    forAll input $ \x@(x1, x2) ->
    forAllBlind inputF $ \f ->
      arr2 f x == f x1 x2
  where
    input :: Gen (Integer, Integer)
    input = arbitrary

    inputF :: Gen (Integer -> Integer -> Integer)
    inputF = arbitrary

testArr3 :: Property
testArr3 =
    forAll input $ \x@(x1, x2, x3) ->
    forAllBlind inputF $ \f ->
      arr3 f x == f x1 x2 x3
  where
    input :: Gen (Integer, Integer, Integer)
    input = arbitrary

    inputF :: Gen (Integer -> Integer -> Integer -> Integer)
    inputF = arbitrary

testArr4 :: Property
testArr4 =
    forAll input $ \x@(x1, x2, x3, x4) ->
    forAllBlind inputF $ \f ->
      arr4 f x == f x1 x2 x3 x4
  where
    input :: Gen (Integer, Integer, Integer, Integer)
    input = arbitrary

    inputF :: Gen (Integer -> Integer -> Integer -> Integer -> Integer)
    inputF = arbitrary

testArr5 :: Property
testArr5 =
    forAll input $ \x@(x1, x2, x3, x4, x5) ->
    forAllBlind inputF $ \f ->
      arr5 f x == f x1 x2 x3 x4 x5
  where
    input :: Gen (Integer, Integer, Integer, Integer, Integer)
    input = arbitrary

    inputF :: Gen (  Integer
                  -> Integer
                  -> Integer
                  -> Integer
                  -> Integer
                  -> Integer
                  )
    inputF = arbitrary
