-- |
-- Description : Test cases for FRP.Yampa.InternalCore
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson, Ivan Perez
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
  , testProperty "Arrow Naturality"                       prop_arr_naturality
  , testProperty "composition (0, fixed)"                 (property $ comp_t0 ~= comp_t0r)
  , testProperty "composition (1, fixed)"                 (property $ comp_t1 ~= comp_t1r)
  , testProperty "composition (2, fixed)"                 (property $ comp_t2 ~= comp_t2r)
  , testProperty "composition (3, fixed)"                 (property $ comp_t3 ~= comp_t3r)
  , testProperty "composition (4, fixed)"                 (property $ comp_t4 ~= comp_t4r)
  , testProperty "composition (5, fixed)"                 (property $ comp_t5 ~= comp_t5r)
  , testProperty "Arrows > Composition (1)"               prop_arrow_comp_1
  , testProperty "Arrows > Composition (2)"               prop_arrow_comp_2
  , testProperty "Arrows > Composition (3)"               prop_arrow_comp_3
  , testProperty "first (0, fixed)"                       (property $ first_t0 ~= first_t0r)
  , testProperty "first (1, fixed)"                       (property $ first_t1 ~= first_t1r)
  , testProperty "first (2, fixed)"                       (property $ first_t2 ~= first_t2r)
  , testProperty "first (3, fixed)"                       (property $ first_t3 ~= first_t3r)
  , testProperty "first (4, fixed)"                       (property $ first_t4 ~= first_t4r)
  , testProperty "first (5, fixed)"                       (property $ first_t5 ~= first_t5r)
  , testProperty "Arrows > First (1)"                     prop_arrow_first_1
  , testProperty "Arrows > First (2)"                     prop_arrow_first_2
  , testProperty "second (0, fixed)"                      (property $ second_t0 ~= first_t0r)
  , testProperty "second (1, fixed)"                      (property $ second_t1 ~= first_t1r)
  , testProperty "second (2, fixed)"                      (property $ second_t2 ~= first_t2r)
  , testProperty "second (3, fixed)"                      (property $ second_t3 ~= first_t3r)
  , testProperty "second (4, fixed)"                      (property $ second_t4 ~= first_t4r)
  , testProperty "second (5, fixed)"                      (property $ second_t5 ~= first_t5r)
  , testProperty "Arrows > Second (1)"                    prop_arrow_second_1
  , testProperty "Arrows > Second (2)"                    prop_arrow_second_2
  , testProperty "arrow laws (0, fixed)"                  (property $ laws_t0_lhs ~= laws_t0_rhs)
  , testProperty "Arrows > Identity (0)"                  prop_arrow_id_0
  , testProperty "arrow laws (1, fixed)"                  (property $ laws_t1_lhs ~= laws_t1_rhs)
  , testProperty "Arrows > Identity (2)"                  prop_arrow_id_2
  , testProperty "arrow laws (2, fixed)"                  (property $ laws_t2_lhs ~= laws_t2_rhs)
  , testProperty "Arrows > Associativity"                 prop_arrow_assoc
  , testProperty "arrow laws (3, fixed)"                  (property $ laws_t3_lhs ~= laws_t3_rhs)
  , testProperty "Arrows > Function lifting composition"  prop_arrow_arr_comp
  , testProperty "arrow laws (4, fixed)"                  (property $ laws_t4_lhs ~= laws_t4_rhs)
  , testProperty "Arrows > First"                         prop_arrow_first_3
  , testProperty "arrow laws (5, fixed)"                  (property $ laws_t5_lhs ~= laws_t5_rhs)
  , testProperty "Arrows > Distributivity of First"       prop_arrow_first_distrib
  , testProperty "arrow laws (6, fixed)"                  (property $ laws_t6_lhs ~= laws_t6_rhs)
  , testProperty "Arrows > Commutativity of id on first"  prop_arrow_first_id_comm
  , testProperty "arrow laws (7, fixed)"                  (property $ laws_t7_lhs ~= laws_t7_rhs)
  , testProperty "arrow laws (8, fixed)"                  (property $ laws_t8_lhs ~= laws_t8_rhs)
  , testProperty "Arrows > Nested firsts"                 prop_arrow_first_nested
  , testProperty "loop (0, fixed)"                        (property $ loop_t0  ~= loop_t0r)
  , testProperty "loop (1, fixed)"                        (property $ loop_t1  ~= loop_t1r)
  , testProperty "loop (2, fixed)"                        (property $ loop_t2  ~= loop_t2r)
  , testProperty "loop (3, fixed)"                        (property $ loop_t3  ~= loop_t3r)
  , testProperty "loop (4, fixed)"                        (property $ loop_t4  ~= loop_t4r)
  , testProperty "loop (5, fixed)"                        (property $ loop_t5  ~= loop_t5r)
  , testProperty "loop (6, fixed)"                        (property $ loop_t6  ~= loop_t6r)
  , testProperty "loop (7, fixed)"                        (property $ loop_t7  ~= loop_t7r)
  , testProperty "loop (8, fixed)"                        (property $ loop_t8  ~= loop_t8r)
  , testProperty "loop (9, fixed)"                        (property $ loop_t9  ~= loop_t9r)
  , testProperty "loop (10, fixed)"                       (property $ loop_t10 ~= loop_t10r)
  , testProperty "loop (11, fixed)"                       (property $ loop_t11 ~= loop_t11r)
  , testProperty "loop (12, fixed)"                       (property $ loop_t12 ~= loop_t12r)
  , testProperty "loop (13, fixed)"                       (property $ loop_t13 ~= loop_t13r)
  , testProperty "loop (14, fixed)"                       (property $ loop_t14 ~= loop_t14r)
  , testProperty "loop (15, fixed)"                       (property $ loop_t15 ~= loop_t15r)
  , testProperty "loop (16, fixed)"                       (property $ loop_t16 ~= loop_t16r)
  , testProperty "loop (17, fixed)"                       (property $ loop_t17 ~= loop_t17r)
  , testProperty "loop laws (0, fixed)"                   (property $ looplaws_t0_lhs  ~= looplaws_t0_rhs)
  , testProperty "loop laws (1, fixed)"                   (property $ looplaws_t1_lhs  ~= looplaws_t1_rhs)
  , testProperty "loop laws (2, fixed)"                   (property $ looplaws_t2_lhs  ~= looplaws_t2_rhs)
  , testProperty "loop laws (3, fixed)"                   (property $ looplaws_t3_lhs  ~= looplaws_t3_rhs)
  , testProperty "loop laws (4, fixed)"                   (property $ looplaws_t4_lhs  ~= looplaws_t4_rhs)
  , testProperty "loop laws (5, fixed)"                   (property $ looplaws_t5_lhs  ~= looplaws_t5_rhs)
  ]

-- * Arrow instance and implementation

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

prop_arrow_1 = forAll myStream $ evalT $
    Always $ prop (arr id, \x y -> x == y)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

-- C1: Arr naturality (testSF1 (arr (+1)))
prop_arr_naturality =
    forAll myStream $ \stream ->
      forAll f $ \f' ->
        evalT (Always (prop (arr (apply f'), \x y -> apply f' x == y)))
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream
    f :: Gen (Fun Int Int)
    f = arbitrary

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

-- Arrow composition (we use Int to avoid floating-point discrepancies)
prop_arrow_comp_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    sf   = arr (+1) >>> arr (+2)
    pred = (\x y -> x + 3 == y)

-- Arrow composition
prop_arrow_comp_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = uniDistStream

    sf   = constant 5.0 >>> arr (+1)
    pred = const (== 6.0)

-- Arrow composition
prop_arrow_comp_3 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Float)
    myStream = fixedDelayStream 0.25

    sf :: SF a Float
    sf = constant 2.0 >>> integral >>> stepDiff (-0.5)

    pred = const (== 0.5)

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

prop_arrow_first_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    sf   = arr dup >>> first (constant 7)
    pred = (\x y -> (7 :: Int, x) == y)

prop_arrow_first_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    sf   = arr dup >>> first (arr (+1))
    pred = (\x y -> (x + 1, x) == y)

-- Test cases for second
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

prop_arrow_second_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    sf   = arr dup >>> second (constant 7)
    pred = (\x y -> (x, 7 :: Int) == y)

prop_arrow_second_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    sf   = arr dup >>> second (arr (+1))
    pred = (\x y -> (x, x + 1) == y)

-- For a description of the laws, see e.g. Ross Paterson: Embedding a Class of
-- Domain-Specific Languages in a Functional Language.
-- Only a very rudimentary sanity check. Obviously not intended to "prove"
-- this implementation indeed do respect the laws.

laws_t0_lhs :: [Double]
laws_t0_lhs = testSF1 (arr id >>> integral)

laws_t0_rhs :: [Double]
laws_t0_rhs = testSF1 (integral)

prop_arrow_id_0 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where
    sf1 = arr id >>> integral
    sf2 = integral
    pred = arr $ uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

laws_t1_lhs :: [Double]
laws_t1_lhs = testSF1 (integral >>> arr id)
laws_t1_rhs :: [Double]
laws_t1_rhs = testSF1 (integral)

prop_arrow_id_2 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where
    sf1 = integral >>> arr id
    sf2 = integral
    pred = arr $ uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

laws_t2_lhs :: [Double]
laws_t2_lhs = testSF1 ((integral >>> arr (*0.5)) >>> integral)
laws_t2_rhs :: [Double]
laws_t2_rhs = testSF1 (integral >>> (arr (*0.5) >>> integral))

prop_arrow_assoc =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where
    sf1 = (integral >>> arr (*0.5)) >>> integral
    sf2 = integral >>> (arr (*0.5) >>> integral)
    pred = arr $ uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

laws_t3_lhs :: [Double]
laws_t3_lhs = testSF1 (arr ((*2.5) . (+3.0)))
laws_t3_rhs :: [Double]
laws_t3_rhs = testSF1 (arr (+3.0) >>> arr (*2.5))

prop_arrow_arr_comp =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where
    sf1 = (arr ((*2.5) . (+3.0)))
    sf2 = (arr (+3.0) >>> arr (*2.5))
    pred = arr (uncurry (==))

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

prop_arrow_2 = forAll myStream $ evalT $
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where
    myStream :: Gen (SignalSampleStream Float)
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

  where
    myStream :: Gen (SignalSampleStream Int)
    myStream = uniDistStream

    f, g :: Gen (Fun Int Int)
    f = arbitrary
    g = arbitrary

prop_arrow_2'' f g =
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where
    sf1 = arr (f >>> g)
    sf2 = arr f >>> arr g

laws_t4_lhs :: [(Double, Double)]
laws_t4_lhs = testSF1 (arr dup >>> first (arr (*2.5)))
laws_t4_rhs :: [(Double, Double)]
laws_t4_rhs = testSF1 (arr dup >>> arr ((*2.5) *** id))

prop_arrow_first_3 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr dup >>> first (arr (*2.5)))
    sf2 = (arr dup >>> arr (fun_prod (*2.5) id))
    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

laws_t5_lhs :: [(Double, Double)]
laws_t5_lhs = testSF1 (arr dup >>> (first (integral >>> arr (+3.0))))
laws_t5_rhs :: [(Double, Double)]
laws_t5_rhs = testSF1 (arr dup >>> (first integral >>> first (arr (+3.0))))

prop_arrow_first_distrib =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr dup >>> (first (integral >>> arr (+3.0))))
    sf2 = (arr dup >>> (first integral >>> first (arr (+3.0))))
    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

laws_t6_lhs :: [(Double, Double)]
laws_t6_lhs = testSF1 (arr dup >>> (first integral >>> arr (id *** (+3.0))))
laws_t6_rhs :: [(Double, Double)]
laws_t6_rhs = testSF1 (arr dup >>> (arr (id *** (+3.0)) >>> first integral))

prop_arrow_first_id_comm =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr dup >>> (first integral>>>arr (fun_prod id (+3.0))))
    sf2 = (arr dup >>> (arr (fun_prod id (+3.0))>>>first integral))
    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

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

prop_arrow_first_nested =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr (\x -> ((x,x),())) >>> (first (first integral) >>> arr assoc))
    sf2 = (arr (\x -> ((x,x),())) >>> (arr assoc >>> first integral))

    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

-- * Test cases for loop

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

loop_t0 :: [Double]
loop_t0 = testSF1 (loop (constant (42.0, 43.0)))
loop_t0r =
  [ 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0, 42.0
  , 42.0, 42.0, 42.0, 42.0, 42.0
  ]

loop_t1 :: [Double]
loop_t1 = testSF1 (loop identity)
loop_t1r =
  [ 0.0,  1.0,  2.0,  3.0,  4.0,  5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

loop_t2 :: [Time]
loop_t2 = testSF1 (loop (first localTime))
loop_t2r =
  [ 0.0,  0.25, 0.5,  0.75, 1.0
  , 1.25, 1.5,  1.75, 2.0,  2.25
  , 2.5,  2.75, 3.0,  3.25, 3.5
  , 3.75, 4.0,  4.25, 4.5,  4.75
  , 5.0,  5.25, 5.5,  5.75, 6.0
  ]

-- AC, 10-March-2002: I think this is the simplest test that will
-- fail with AltST.
loop_t3 :: [Time]
loop_t3 = testSF1 (loop (second (iPre 0)))
loop_t3r =
  [ 0.0,  1.0,  2.0,  3.0,  4.0
  , 5.0,  6.0,  7.0,  8.0,  9.0
  , 10.0, 11.0, 12.0, 13.0, 14.0
  , 15.0, 16.0, 17.0, 18.0, 19.0
  , 20.0, 21.0, 22.0, 23.0, 24.0
  ]

loop_t4 :: [Double]
loop_t4 = testSF1 (loop (second (iPre 0) >>> loop_acc))
loop_t4r =
  [ 0.0,   1.0,   3.0,   6.0,   10.0
  , 15.0,  21.0,  28.0,  36.0,  45.0
  , 55.0,  66.0,  78.0,  91.0,  105.0
  , 120.0, 136.0, 153.0, 171.0, 190.0
  , 210.0, 231.0, 253.0, 276.0, 300.0
  ]

loop_t5 :: [Double]
loop_t5 = testSF2 (loop (second (iPre 0) >>> loop_acc))
loop_t5r =
  [ 0.0,  0.0,  0.0,  0.0,  0.0
  , 1.0,  2.0,  3.0,  4.0,  5.0
  , 7.0,  9.0,  11.0, 13.0, 15.0
  , 18.0, 21.0, 24.0, 27.0, 30.0
  , 34.0, 38.0, 42.0, 46.0, 50.0
  ]

loop_t6 :: [Double]
loop_t6 = testSF1 (loop (iPre (0,0) >>> first localTime >>> loop_acc))
loop_t6r =
  [ 0.0,   0.25,  0.75,  1.5,   2.5
  , 3.75,  5.25,  7.0,   9.0,   11.25
  , 13.75, 16.5,  19.5,  22.75, 26.25
  , 30.0,  34.0,  38.25, 42.75, 47.5
  , 52.5,  57.75, 63.25, 69.0,  75.0
  ]

loop_t7 :: [Double]
loop_t7 = testSF1 (loop (loop_acc >>> second (iPre 0)))
loop_t7r = loop_t4r

loop_t8 :: [Double]
loop_t8 = testSF2 (loop (loop_acc >>> second (iPre 0)))
loop_t8r = loop_t5r

loop_t9 :: [Double]
loop_t9 = testSF1 (loop (first localTime >>> loop_acc >>> iPre (0,0)))
loop_t9r =
  [ 0.0,   0.0,   0.25,  0.75,  1.5
  , 2.5,   3.75,  5.25,  7.0,   9.0
  , 11.25, 13.75, 16.5,  19.5,  22.75
  , 26.25, 30.0,  34.0,  38.25, 42.75
  , 47.5,  52.5,  57.75, 63.25, 69.0
  ]

loop_t10 :: [Double]
loop_t10 = testSF1 (loop (loop_acc >>> second (iPre 0) >>> identity))
loop_t10r = loop_t4r

loop_t11 :: [Double]
loop_t11 = testSF2 (loop (loop_acc >>> second (iPre 0) >>> identity))
loop_t11r = loop_t5r

loop_t12 :: [Double]
loop_t12 = testSF1 (loop (first localTime
                          >>> loop_acc
                          >>> iPre (0,0)
                          >>> identity))
loop_t12r = loop_t9r

-- Computation of approximation to exp 0, exp 1, ..., exp 5 by integration.
-- Values as given by using exp directly:
-- 1.0, 2.71828, 7.38906, 20.0855, 54.5981, 148.413
loop_t13 :: [Double]
loop_t13 =
  let es = embed (loop (second integral >>> arr (\(_, x) -> (x + 1, x + 1))))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]

loop_t13r = [1.0,2.71692, 7.38167, 20.05544, 54.48911, 148.04276]

loop_t14 :: [Double]
loop_t14 =
  let es = embed (loop (arr (\(_, x) -> (x + 1, x + 1)) >>> second integral))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t14r = loop_t13r

loop_t15 :: [Double]
loop_t15 =
  let es = embed (loop (arr (\(_, x) -> (x + 1, x + 1))
                        >>> second integral
                        >>> identity))
                 (deltaEncode 0.001 (repeat ()))
  in [es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loop_t15r = loop_t13r

-- A generator for factorial:  The least-fixed point of this function is
-- the factorial function.

factGen f n = if (n==0) then 1 else n*f(n-1)

-- Can we use loop to construct a fixed point?
loop_t16 :: [Int]
loop_t16 = testSF1 (loop $ arr (\ (_,f) -> (f 4,factGen f)))
loop_t16r =
  [24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24]

-- A simple loop test taken from MiniYampa:
-- This results in pulling on the fed-back output during evaluation, because
-- switch is strict in its input sample:
loop_t17 :: [Double]
loop_t17 = testSF1 (loop $ second $ (switch identity (const (arr fst))) >>> arr (\x -> (x,noEvent)) >>> (iPre (25, noEvent)))
loop_t17r =
  [ 0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0
  , 16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0
  ]

-- For a description of the laws, see Ross Paterson: Embedding a Class of
-- Domain-Specific Languages in a Functional Language.
-- Only a very rudimentary sanity check. Obviously not intended to "prove"
-- this implementation indeed do respect the laws.

simple_loop :: ((a,c) -> (b,c)) -> (a -> b)
simple_loop f a = b
  where
    (b, c) = f (a, c)

-- Left tightening
looplaws_t0_f = second integral >>> arr swap
looplaws_t0_h :: Fractional a => SF a a
looplaws_t0_h = arr (+10.0)
looplaws_t0_lhs :: [Double]
looplaws_t0_lhs = testSF1 (loop (first looplaws_t0_h >>> looplaws_t0_f))
looplaws_t0_rhs :: [Double]
looplaws_t0_rhs = testSF1 (looplaws_t0_h >>> loop looplaws_t0_f)

-- Right tightening
looplaws_t1_f = second integral >>> arr swap
looplaws_t1_h :: Fractional a => SF a a
looplaws_t1_h = arr (+10.0)
looplaws_t1_lhs :: [Double]
looplaws_t1_lhs = testSF1 (loop (looplaws_t1_f >>> first looplaws_t1_h))
looplaws_t1_rhs :: [Double]
looplaws_t1_rhs = testSF1 (loop looplaws_t1_f >>> looplaws_t1_h)

-- Sliding
-- Used to work with only signature t2_f :: Fractional a -> SF a a
looplaws_t2_f :: SF (Double, Double) (Double, Double)
looplaws_t2_f = integral
looplaws_t2_k = id *** (+42.0)
looplaws_t2_lhs :: [Double]
looplaws_t2_lhs = testSF1 (loop (looplaws_t2_f >>> arr looplaws_t2_k))
looplaws_t2_rhs :: [Double]
looplaws_t2_rhs = testSF1 (loop (arr looplaws_t2_k >>> looplaws_t2_f))

-- Vanishing
-- The lazy pattern matching (~) is necessary to avoid a black hole in the
-- RHS due to premature forcing of tuples. As far as I can tell, loop is
-- as lazy as it can be, and this problem could not have been solved by
-- "fixing" the loop definition.
looplaws_t3_f = second integral
                >>> first (arr swap)
                >>> arr (\ ~((a,b),c) -> ((a,c),b))
looplaws_t3_lhs :: [Double]
looplaws_t3_lhs = testSF1 (loop (loop looplaws_t3_f))
looplaws_t3_rhs :: [Double]
looplaws_t3_rhs = testSF1 (loop (arr assocInv >>> looplaws_t3_f >>> arr assoc))

-- Superposing
looplaws_t4_f = second integral >>> arr swap
looplaws_t4_lhs :: [(Double,Double)]
looplaws_t4_lhs = testSF1 (arr dup >>> (second (loop looplaws_t4_f)))
looplaws_t4_rhs :: [(Double, Double)]
looplaws_t4_rhs = testSF1 (arr dup >>> (loop (arr assoc
                                        >>> second looplaws_t4_f
                                        >>> arr assocInv)))

-- Extension
looplaws_t5_f = \(a,c) -> (take 5 c, a : c)
looplaws_t5_lhs :: [[Double]]
looplaws_t5_lhs = testSF1 (loop (arr looplaws_t5_f))
looplaws_t5_rhs :: [[Double]]
looplaws_t5_rhs = testSF1 (arr (simple_loop looplaws_t5_f))

-- * Auxiliary

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))
