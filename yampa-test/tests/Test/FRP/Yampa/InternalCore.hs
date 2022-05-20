-- |
-- Description : Test cases for FRP.Yampa.InternalCore
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.InternalCore
    ( tests
    )
  where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Data.Tuple (swap)

import FRP.Yampa as Yampa
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.InternalCore"
  [ testProperty "arr (0, fixed)"                         (property $ arr_t0 ~= arr_t0r)
  , testProperty "arr (1, fixed)"                         (property $ arr_t1 ~= arr_t1r)
  , testProperty "composition (0, fixed)"                 (property $ comp_t0 ~= comp_t0r)
  , testProperty "composition (1, fixed)"                 (property $ comp_t1 ~= comp_t1r)
  , testProperty "composition (2, fixed)"                 (property $ comp_t2 ~= comp_t2r)
  , testProperty "composition (3, fixed)"                 (property $ comp_t3 ~= comp_t3r)
  , testProperty "composition (4, fixed)"                 (property $ comp_t4 ~= comp_t4r)
  , testProperty "composition (5, fixed)"                 (property $ comp_t5 ~= comp_t5r)
  , testProperty "first (0, fixed)"                       (property $ first_t0 ~= first_t0r)
  , testProperty "first (1, fixed)"                       (property $ first_t1 ~= first_t1r)
  , testProperty "first (2, fixed)"                       (property $ first_t2 ~= first_t2r)
  , testProperty "first (3, fixed)"                       (property $ first_t3 ~= first_t3r)
  , testProperty "first (4, fixed)"                       (property $ first_t4 ~= first_t4r)
  , testProperty "first (5, fixed)"                       (property $ first_t5 ~= first_t5r)
  , testProperty "second (0, fixed)"                      (property $ second_t0 ~= first_t0r)
  , testProperty "second (1, fixed)"                      (property $ second_t1 ~= first_t1r)
  , testProperty "second (2, fixed)"                      (property $ second_t2 ~= first_t2r)
  , testProperty "second (3, fixed)"                      (property $ second_t3 ~= first_t3r)
  , testProperty "second (4, fixed)"                      (property $ second_t4 ~= first_t4r)
  , testProperty "second (5, fixed)"                      (property $ second_t5 ~= first_t5r)
  , testProperty "arrow laws (0, fixed)"                  (property $ laws_t0_lhs ~= laws_t0_rhs)
  , testProperty "arrow laws (1, fixed)"                  (property $ laws_t1_lhs ~= laws_t1_rhs)
  , testProperty "arrow laws (2, fixed)"                  (property $ laws_t2_lhs ~= laws_t2_rhs)
  , testProperty "arrow laws (3, fixed)"                  (property $ laws_t3_lhs ~= laws_t3_rhs)
  , testProperty "arrow laws (4, fixed)"                  (property $ laws_t4_lhs ~= laws_t4_rhs)
  , testProperty "arrow laws (5, fixed)"                  (property $ laws_t5_lhs ~= laws_t5_rhs)
  , testProperty "arrow laws (6, fixed)"                  (property $ laws_t6_lhs ~= laws_t6_rhs)
  , testProperty "arrow laws (7, fixed)"                  (property $ laws_t7_lhs ~= laws_t7_rhs)
  , testProperty "arrow laws (8, fixed)"                  (property $ laws_t8_lhs ~= laws_t8_rhs)
  , testProperty "Arrow Naturality"                       prop_arr_naturality
  , testProperty "Naturality"                             prop_arr_naturality
  , testProperty "Arrows > Composition (1)"               prop_arrow_comp_1
  , testProperty "Arrows > Composition (2)"               prop_arrow_comp_2
  , testProperty "Arrows > Composition (3)"               prop_arrow_comp_3
  , testProperty "Arrows > First (1)"                     prop_arrow_first_1
  , testProperty "Arrows > First (2)"                     prop_arrow_first_2
  , testProperty "Arrows > Second (1)"                    prop_arrow_second_1
  , testProperty "Arrows > Second (2)"                    prop_arrow_second_2
  , testProperty "Arrows > Identity (0)"                  prop_arrow_id_0
  , testProperty "Arrows > Identity (2)"                  prop_arrow_id_2
  , testProperty "Arrows > Associativity"                 prop_arrow_assoc
  , testProperty "Arrows > Function lifting composition"  prop_arrow_arr_comp
  , testProperty "Arrows > First"                         prop_arrow_first_3
  , testProperty "Arrows > Distributivity of First"       prop_arrow_first_distrib
  , testProperty "Arrows > Commutativity of id on first"  prop_arrow_first_id_comm
  , testProperty "Arrows > Nested firsts"                 prop_arrow_first_nested
  ]

-- * Test cases for arr

arr_t0 = testSF1 (arr (+1))
arr_t0r =
  [ 1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0
  , 17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0
  ]

arr_t1 = testSF2 (arr (+1))
arr_t1r =
  [ 1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0
  , 4.0,4.0,5.0,5.0,5.0,5.0,5.0
  ]

-- * Test cases for comp

comp_t0 = testSF1 ((arr (+1)) >>> (arr (+2)))
comp_t0r :: [Double]
comp_t0r =
  [ 3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0
  , 18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0
  ]

comp_t1 = testSF2 ((arr (+1)) >>> (arr (+2)))
comp_t1r :: [Double]
comp_t1r =
  [ 3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0,4.0,5.0,5.0,5.0,5.0,5.0
  , 6.0,6.0,6.0,6.0,6.0,7.0,7.0,7.0,7.0,7.0
  ]

comp_t2 = testSF1 ((constant 5.0) >>> (arr (+1)))
comp_t2r :: [Double]
comp_t2r =
  [ 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  , 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  ]

comp_t3 = testSF2 ((constant 5.0) >>> (arr (+1)))
comp_t3r :: [Double]
comp_t3r =
  [ 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  , 6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0,6.0
  ]

-- Integration by the rectangle rule or trapezoid rule makes no difference.
comp_t4 = testSF1 ((constant 2.0) >>> integral)
comp_t4r :: [Double]
comp_t4r =
  [ 0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5
  , 9.0,9.5,10.0,10.5,11.0,11.5,12.0
  ]

-- Same result as above.
comp_t5 = testSF2 ((constant 2.0) >>> integral)
comp_t5r :: [Double]
comp_t5r =
  [ 0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0,8.5
  , 9.0,9.5,10.0,10.5,11.0,11.5,12.0
  ]

-- * Test cases for first

first_t0 :: [(Int,Double)]
first_t0 = testSF1 (arr dup >>> first (constant 7))
first_t0r :: [(Int,Double)]
first_t0r =
  [ (7,0.0),  (7,1.0),  (7,2.0),  (7,3.0),  (7,4.0)
  , (7,5.0),  (7,6.0),  (7,7.0),  (7,8.0),  (7,9.0)
  , (7,10.0), (7,11.0), (7,12.0), (7,13.0), (7,14.0)
  , (7,15.0), (7,16.0), (7,17.0), (7,18.0), (7,19.0)
  , (7,20.0), (7,21.0), (7,22.0), (7,23.0), (7,24.0)
  ]

first_t1 :: [(Int,Double)]
first_t1 = testSF2 (arr dup >>> first (constant 7))
first_t1r :: [(Int,Double)]
first_t1r =
  [ (7,0.0), (7,0.0), (7,0.0), (7,0.0), (7,0.0)
  , (7,1.0), (7,1.0), (7,1.0), (7,1.0), (7,1.0)
  , (7,2.0), (7,2.0), (7,2.0), (7,2.0), (7,2.0)
  , (7,3.0), (7,3.0), (7,3.0), (7,3.0), (7,3.0)
  , (7,4.0), (7,4.0), (7,4.0), (7,4.0), (7,4.0)
  ]

first_t2 :: [(Double,Double)]
first_t2 = testSF1 (arr dup >>> first (arr (+1)))
first_t2r =
  [ (1.0,0.0),   (2.0,1.0),   (3.0,2.0),   (4.0,3.0),   (5.0,4.0)
  , (6.0,5.0),   (7.0,6.0),   (8.0,7.0),   (9.0,8.0),   (10.0,9.0)
  , (11.0,10.0), (12.0,11.0), (13.0,12.0), (14.0,13.0), (15.0,14.0)
  , (16.0,15.0), (17.0,16.0), (18.0,17.0), (19.0,18.0), (20.0,19.0)
  , (21.0,20.0), (22.0,21.0), (23.0,22.0), (24.0,23.0), (25.0,24.0)
  ]

first_t3 :: [(Double,Double)]
first_t3 = testSF2 (arr dup >>> first (arr (+1)))
first_t3r =
  [ (1.0,0.0), (1.0,0.0), (1.0,0.0), (1.0,0.0), (1.0,0.0)
  , (2.0,1.0), (2.0,1.0), (2.0,1.0), (2.0,1.0), (2.0,1.0)
  , (3.0,2.0), (3.0,2.0), (3.0,2.0), (3.0,2.0), (3.0,2.0)
  , (4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0)
  , (5.0,4.0), (5.0,4.0), (5.0,4.0), (5.0,4.0), (5.0,4.0)
  ]

first_t4 :: [(Double,Double)]
first_t4 = testSF1 (arr dup >>> first integral)
first_t4r =
  [ (0.0,0.0),    (0.0,1.0),    (0.25,2.0),   (0.75,3.0),   (1.5,4.0)
  , (2.5,5.0),    (3.75,6.0),   (5.25,7.0),   (7.0,8.0),    (9.0,9.0)
  , (11.25,10.0), (13.75,11.0), (16.5,12.0),  (19.5,13.0),  (22.75,14.0)
  , (26.25,15.0), (30.0,16.0),  (34.0,17.0),  (38.25,18.0), (42.75,19.0)
  , (47.5,20.0),  (52.5,21.0),  (57.75,22.0), (63.25,23.0), (69.0,24.0)
  ]

first_t5 :: [(Double,Double)]
first_t5 = testSF2 (arr dup >>> first integral)
first_t5r =
  [ (0.0,0.0),  (0.0,0.0),  (0.0,0.0),  (0.0,0.0),  (0.0,0.0)
  , (0.0,1.0),  (0.25,1.0), (0.5,1.0),  (0.75,1.0), (1.0,1.0)
  , (1.25,2.0), (1.75,2.0), (2.25,2.0), (2.75,2.0), (3.25,2.0)
  , (3.75,3.0), (4.5,3.0),  (5.25,3.0), (6.0,3.0),  (6.75,3.0)
  , (7.5,4.0),  (8.5,4.0),  (9.5,4.0),  (10.5,4.0), (11.5,4.0)
  ]

------------------------------------------------------------------------------
-- Test cases for second
------------------------------------------------------------------------------

-- These should mirror the test cases for first.

second_t0 :: [(Int,Double)]
second_t0 = testSF1 (arr dup >>> second (constant 7) >>> arr swap)

second_t1 :: [(Int,Double)]
second_t1 = testSF2 (arr dup >>> second (constant 7) >>> arr swap)

second_t2 :: [(Double,Double)]
second_t2 = testSF1 (arr dup >>> second (arr (+1)) >>> arr swap)

second_t3 :: [(Double,Double)]
second_t3 = testSF2 (arr dup >>> second (arr (+1)) >>> arr swap)

second_t4 :: [(Double,Double)]
second_t4 = testSF1 (arr dup >>> second integral >>> arr swap)

second_t5 :: [(Double,Double)]
second_t5 = testSF2 (arr dup >>> second integral >>> arr swap)

-- * Test cases based on the arrow laws

-- For a description of the laws, see e.g. Ross Paterson: Embedding a Class of
-- Domain-Specific Languages in a Functional Language.
-- Only a very rudimentary sanity check. Obviously not intended to "prove"
-- this implementation indeed do respect the laws.

laws_t0_lhs :: [Double]
laws_t0_lhs = testSF1 (arr id >>> integral)
laws_t0_rhs :: [Double]
laws_t0_rhs = testSF1 (integral)

laws_t1_lhs :: [Double]
laws_t1_lhs = testSF1 (integral >>> arr id)
laws_t1_rhs :: [Double]
laws_t1_rhs = testSF1 (integral)

laws_t2_lhs :: [Double]
laws_t2_lhs = testSF1 ((integral >>> arr (*0.5)) >>> integral)
laws_t2_rhs :: [Double]
laws_t2_rhs = testSF1 (integral >>> (arr (*0.5) >>> integral))

laws_t3_lhs :: [Double]
laws_t3_lhs = testSF1 (arr ((*2.5) . (+3.0)))
laws_t3_rhs :: [Double]
laws_t3_rhs = testSF1 (arr (+3.0) >>> arr (*2.5))

laws_t4_lhs :: [(Double, Double)]
laws_t4_lhs = testSF1 (arr dup >>> first (arr (*2.5)))
laws_t4_rhs :: [(Double, Double)]
laws_t4_rhs = testSF1 (arr dup >>> arr ((*2.5) *** id))

laws_t5_lhs :: [(Double, Double)]
laws_t5_lhs = testSF1 (arr dup >>> (first (integral >>> arr (+3.0))))
laws_t5_rhs :: [(Double, Double)]
laws_t5_rhs = testSF1 (arr dup >>> (first integral >>> first (arr (+3.0))))

laws_t6_lhs :: [(Double, Double)]
laws_t6_lhs = testSF1 (arr dup >>> (first integral >>> arr (id *** (+3.0))))
laws_t6_rhs :: [(Double, Double)]
laws_t6_rhs = testSF1 (arr dup >>> (arr (id *** (+3.0)) >>> first integral))

laws_t7_lhs :: [Double]
laws_t7_lhs = testSF1 (arr dup >>> (first integral >>> arr fst))
laws_t7_rhs :: [Double]
laws_t7_rhs = testSF1 (arr dup >>> (arr fst >>> integral))

laws_t8_lhs :: [(Double, (Double, ()))]
laws_t8_lhs = testSF1 (arr (\x -> ((x,x),()))
                       >>> (first (first integral) >>> arr assoc))
laws_t8_rhs :: [(Double, (Double, ()))]
laws_t8_rhs = testSF1 (arr (\x -> ((x,x),()))
                       >>> (arr assoc >>> first integral))

-- ** Arrow laws

-- C1: Arr naturality (testSF1 (arr (+1)))
-- C2: Arr naturality (testSF2 (arr (+1)))
prop_arr_naturality =
    forAll myStream $ \stream ->
      forAll f $ \f' ->
        evalT (Always (prop (arr (apply f'), \x y -> apply f' x == y)))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        f :: Gen (Fun Int Int)
        f = arbitrary

-- Arrow composition (we use Int to avoid floating-point discrepancies)
prop_arrow_comp_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr (+1) >>> arr (+2)
        pred = (\x y -> x + 3 == y)

-- Arrow composition
prop_arrow_comp_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = constant 5.0 >>> arr (+1)
        pred = const (== 6.0)

-- Arrow composition
prop_arrow_comp_3 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = fixedDelayStream 0.25

        sf :: SF a Float
        sf = constant 2.0 >>> integral >>> stepDiff (-0.5)

        pred = const (== 0.5)

prop_arrow_first_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> first (constant 7)
        pred = (\x y -> (7 :: Int, x) == y)

prop_arrow_first_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> first (arr (+1))
        pred = (\x y -> (x + 1, x) == y)

prop_arrow_second_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> second (constant 7)
        pred = (\x y -> (x, 7 :: Int) == y)

prop_arrow_second_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> second (arr (+1))
        pred = (\x y -> (x, x + 1) == y)

prop_arrow_id_0 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = arr id >>> integral
        sf2 = integral
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_id_2 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = integral >>> arr id
        sf2 = integral
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_assoc =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = (integral >>> arr (*0.5)) >>> integral
        sf2 = integral >>> (arr (*0.5) >>> integral)
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_arr_comp =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = (arr ((*2.5) . (+3.0)))
        sf2 = (arr (+3.0) >>> arr (*2.5))
        pred = arr (uncurry (==))

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_3 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> first (arr (*2.5)))
        sf2 = (arr dup >>> arr (fun_prod (*2.5) id))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_distrib =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> (first (integral >>> arr (+3.0))))
        sf2 = (arr dup >>> (first integral >>> first (arr (+3.0))))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_id_comm =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> (first integral>>>arr (fun_prod id (+3.0))))
        sf2 = (arr dup >>> (arr (fun_prod id (+3.0))>>>first integral))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_nested =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr (\x -> ((x,x),())) >>> (first (first integral) >>> arr assoc))
    sf2 = (arr (\x -> ((x,x),())) >>> (arr assoc >>> first integral))

    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

-- Yampa's Arrow Checks

prop_arrow_1 = forAll myStream $ evalT $
    Always $ prop (arr id, \x y -> x == y)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

prop_arrow_2 = forAll myStream $ evalT $
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        sf1 = arr (f >>> g)
        sf2 = arr f >>> arr g
        f = (+5)
        g = (/20)

prop_arrow_2' =
    forAll f $ \f' ->
      forAll g $ \g' ->
        forAll myStream $ evalT $
          prop_arrow_2'' (apply f') (apply g')

  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        f, g :: Gen (Fun Int Int)
        f = arbitrary
        g = arbitrary

prop_arrow_2'' f g =
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where sf1 = arr (f >>> g)
        sf2 = arr f >>> arr g

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))
