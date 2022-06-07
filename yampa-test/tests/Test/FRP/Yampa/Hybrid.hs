-- |
-- Description : Test cases for hybrid signal functions
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Hybrid
    ( tests
    )
  where

import Data.Maybe (fromJust)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Hybrid as Yampa

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Hybrid"
  [ testProperty "hold (0, fixed)"          (property $ wfg_t0 ~= wfg_t0r)
  , testProperty "hold (1, fixed)"          (property $ wfg_t1 ~= wfg_t1r)
  , testProperty "dHold (0, fixed)"         (property $ utils_t0 ~= utils_t0r)
  , testProperty "dHold (1, fixed)"         (property $ utils_t1 ~= utils_t1r)
  , testProperty "trackAndHold (0, fixed)"  (property $ wfg_t2 ~= wfg_t2r)
  , testProperty "trackAndHold (1, fixed)"  (property $ wfg_t3 ~= wfg_t3r)
  , testProperty "dTrackAndHold (0, fixed)" (property $ utils_t2 ~= utils_t2r)
  , testProperty "dTrackAndHold (1, fixed)" (property $ utils_t3 ~= utils_t3r)
  , testProperty "accum (0, fixed)"         (property $ accum_t0  == accum_t0r)
  , testProperty "accum (1, fixed)"         (property $ accum_t1  == accum_t1r)
  , testProperty "accum (2, fixed)"         (property $ accum_t2  == accum_t2r)
  , testProperty "accum (3, fixed)"         (property $ accum_t3  == accum_t3r)
  , testProperty "accum (8, fixed)"         (property $ accum_t8  == accum_t8r)
  , testProperty "accum (9, fixed)"         (property $ accum_t9  == accum_t9r)
  , testProperty "accum (11, fixed)"        (property $ accum_t11 == accum_t11r)
  , testProperty "accum (10, fixed)"        (property $ accum_t10 == accum_t10r)
  , testProperty "accum (12, fixed)"        (property $ accum_t12 == accum_t12r)
  , testProperty "accum (4, fixed)"         (property $ accum_t4  == accum_t4r)
  , testProperty "accum (5, fixed)"         (property $ accum_t5  == accum_t5r)
  , testProperty "accum (6, fixed)"         (property $ accum_t6  == accum_t6r)
  , testProperty "accum (7, fixed)"         (property $ accum_t7  == accum_t7r)
  , testProperty "accum (13, fixed)"        (property $ accum_t13 == accum_t13r)
  , testProperty "accum (14, fixed)"        (property $ accum_t14 == accum_t14r)
  , testProperty "accum (15, fixed)"        (property $ accum_t15 == accum_t15r)
  , testProperty "accum (16, fixed)"        (property $ accum_t16 == accum_t16r)
  , testProperty "accum (17, fixed)"        (property $ accum_t17 == accum_t17r)
  ]

-- * Wave-form generation

wfg_t0 :: [Double]
wfg_t0 = take 16 $ embed (hold 99.99) wfg_inp1

wfg_t0r =
  [ 99.99, 99.99, 1.0, 1.0
  , 2.0,   2.0,   2.0, 2.0
  , 3.0,   4.0,   4.0, 4.0
  , 0.0,   0.0,   0.0, 0.0
  ]

wfg_inp1 = deltaEncode 1.0 $
  [ NoEvent,   NoEvent,   Event 1.0, NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

wfg_t1 :: [Double]
wfg_t1 = take 16 $ embed (hold 99.99) wfg_inp2

wfg_t1r =
  [ 1.0, 1.0, 1.0, 1.0
  , 2.0, 2.0, 2.0, 2.0
  , 3.0, 4.0, 4.0, 4.0
  , 0.0, 0.0, 0.0, 0.0
  ]

utils_inp1 = deltaEncode 1.0 $
  [ NoEvent,   NoEvent,   Event 1.0, NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

utils_inp2 = deltaEncode 1.0 $
  [ Event 1.0, NoEvent,   NoEvent,   NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

utils_t0 :: [Double]
utils_t0 = take 16 $ embed (dHold 99.99) utils_inp1

utils_t0r =
  [ 99.99, 99.99, 99.99, 1.0
  , 1.0,   2.0,   2.0,   2.0
  , 2.0,   3.0,   4.0,   4.0
  , 4.0,   0.0,   0.0,   0.0
  ]

utils_t1 :: [Double]
utils_t1 = take 16 $ embed (dHold 99.99) utils_inp2

utils_t1r =
  [ 99.99, 1.0, 1.0, 1.0
  , 1.0,   2.0, 2.0, 2.0
  , 2.0,   3.0, 4.0, 4.0
  , 4.0,   0.0, 0.0, 0.0
  ]

wfg_inp2 = deltaEncode 1.0 $
  [ Event 1.0, NoEvent,   NoEvent,   NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 4.0, Event 4.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

wfg_t2 :: [Double]
wfg_t2 = take 25 $ embed (trackAndHold 99.99) wfg_inp3

wfg_t2r =
  [ 99.99, 99.99, 1.0, 2.0, 3.0
  , 4.0,   4.0,   4.0, 4.0, 3.0
  , 2.0,   2.0,   1.0, 0.0, 1.0
  , 2.0,   3.0,   3.0, 3.0, 4.0
  , 4.0,   4.0,   4.0, 4.0, 4.0
  ]

wfg_inp3 = deltaEncode 1.0 $
  [ Nothing,  Nothing,  Just 1.0, Just 2.0, Just 3.0
  , Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0
  , Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0
  , Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0
  ]
  ++ repeat Nothing

wfg_t3 :: [Double]
wfg_t3 = take 25 $ embed (trackAndHold 99.99) wfg_inp4

wfg_t3r =
  [ 0.0, 0.0, 1.0, 2.0, 3.0
  , 4.0, 4.0, 4.0, 4.0, 3.0
  , 2.0, 2.0, 1.0, 0.0, 1.0
  , 2.0, 3.0, 3.0, 3.0, 4.0
  , 4.0, 4.0, 4.0, 4.0, 4.0
  ]

wfg_inp4 = deltaEncode 1.0 $
  [ Just 0.0, Nothing,  Just 1.0, Just 2.0, Just 3.0
  , Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0
  , Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0
  , Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0
  ]
  ++ repeat Nothing

utils_t2 :: [Double]
utils_t2 = take 25 $ embed (dTrackAndHold 99.99) utils_inp3

utils_t2r =
  [ 99.99, 99.99, 99.99, 1.0, 2.0
  , 3.0,   4.0,   4.0,   4.0, 4.0
  , 3.0,   2.0,   2.0,   1.0, 0.0
  , 1.0,   2.0,   3.0,   3.0, 3.0
  , 4.0,   4.0,   4.0,   4.0, 4.0
  ]

utils_inp3 = deltaEncode 1.0 $
  [ Nothing,  Nothing,  Just 1.0, Just 2.0, Just 3.0
  , Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0
  , Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0
  , Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0
  ]
  ++ repeat Nothing

utils_t3 :: [Double]
utils_t3 = take 25 $ embed (dTrackAndHold 99.99) utils_inp4

utils_t3r =
  [ 99.99, 0.0, 0.0, 1.0, 2.0
  , 3.0,   4.0, 4.0, 4.0, 4.0
  , 3.0,   2.0, 2.0, 1.0, 0.0
  , 1.0,   2.0, 3.0, 3.0, 3.0
  , 4.0,   4.0, 4.0, 4.0, 4.0
  ]

utils_inp4 = deltaEncode 1.0 $
  [ Just 0.0, Nothing,  Just 1.0, Just 2.0, Just 3.0
  , Just 4.0, Nothing,  Nothing,  Nothing,  Just 3.0
  , Just 2.0, Nothing,  Just 1.0, Just 0.0, Just 1.0
  , Just 2.0, Just 3.0, Nothing,  Nothing,  Just 4.0
  ]
  ++ repeat Nothing

-- * Accumulators

accum_t0 :: [Event Double]
accum_t0 = take 16 $ embed (accum 0.0) accum_inp1

accum_t0r =
  [ NoEvent,   NoEvent,    Event 1.0,  NoEvent
  , Event 3.0, NoEvent,    NoEvent,    NoEvent
  , Event 9.0, Event 14.0, Event 19.0, NoEvent
  , Event 9.5, NoEvent,    NoEvent,    NoEvent
  ]

accum_inp1 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
  where
    delta_inp =
      [ Just NoEvent, Nothing, Just (Event (+1.0)), Just NoEvent
      , Just (Event (+2.0)), Just NoEvent, Nothing, Nothing
      , Just (Event (*3.0)), Just (Event (+5.0)), Nothing, Just NoEvent
      , Just (Event (/2.0)), Just NoEvent, Nothing, Nothing
      ]
      ++ repeat Nothing

accum_t1 :: [Event Double]
accum_t1 = take 16 $ embed (accum 0.0) accum_inp2

accum_t1r =
  [ Event 1.0, NoEvent,    NoEvent,    NoEvent
  , Event 3.0, NoEvent,    NoEvent,    NoEvent
  , Event 9.0, Event 14.0, Event 19.0, NoEvent
  , Event 9.5, NoEvent,    NoEvent,    NoEvent
  ]

accum_inp2 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
  where
    delta_inp =
      [ Just (Event (+1.0)), Just NoEvent, Nothing, Nothing
      , Just (Event (+2.0)), Just NoEvent, Nothing, Nothing
      , Just (Event (*3.0)), Just (Event (+5.0)), Nothing, Just NoEvent
      , Just (Event (/2.0)), Just NoEvent, Nothing, Nothing
      ]
      ++ repeat Nothing

accum_t2 :: [Event Int]
accum_t2 = take 16 $ embed (accumBy (\a d -> a + floor d) 0) accum_inp3

accum_t2r :: [Event Int]
accum_t2r =
  [ NoEvent,  NoEvent,  Event 1,  NoEvent
  , Event 3,  NoEvent,  NoEvent,  NoEvent
  , Event 6,  Event 11, Event 16, NoEvent
  , Event 16, NoEvent,  NoEvent,  NoEvent
  ]

accum_inp3 = deltaEncode 1.0 $
  [ NoEvent,   NoEvent,   Event 1.0, NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 5.0, Event 5.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

accum_t3 :: [Event Int]
accum_t3 = take 16 $ embed (accumBy (\a d -> a + floor d) 0) accum_inp4

accum_t3r :: [Event Int]
accum_t3r =
  [ Event 1,  NoEvent,  NoEvent,  NoEvent
  , Event 3,  NoEvent,  NoEvent,  NoEvent
  , Event 6,  Event 11, Event 16, NoEvent
  , Event 16, NoEvent,  NoEvent,  NoEvent
  ]

accum_inp4 = deltaEncode 1.0 $
  [ Event 1.0, NoEvent,   NoEvent,   NoEvent
  , Event 2.0, NoEvent,   NoEvent,   NoEvent
  , Event 3.0, Event 5.0, Event 5.0, NoEvent
  , Event 0.0, NoEvent,   NoEvent,   NoEvent
  ]
  ++ repeat NoEvent

accum_t8 :: [Event Int]
accum_t8 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumBy (+) 0
                            >>> accumBy (+) 0)
                           accum_inp5

accum_t8r :: [Event Int]
accum_t8r = [ NoEvent,  NoEvent, NoEvent, NoEvent
            , Event 1,  NoEvent, NoEvent, NoEvent
            , Event 3,  NoEvent, NoEvent, NoEvent
            , Event 6,  NoEvent, NoEvent, NoEvent
            , Event 10, NoEvent, NoEvent, NoEvent
            , Event 15, NoEvent, NoEvent, NoEvent
            , Event 21, NoEvent, NoEvent, NoEvent
            , Event 28, NoEvent, NoEvent, NoEvent
            , Event 36, NoEvent, NoEvent, NoEvent
            , Event 45, NoEvent, NoEvent, NoEvent
            ]

accum_inp5 = deltaEncode 0.25 (repeat ())

accum_t9 :: [Int]
accum_t9 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumBy (+) 0
                            >>> accumBy (+) 0
                            >>> hold 0)
                           accum_inp5

accum_t9r :: [Int]
accum_t9r = [ 0,0,0,0,1,1,1,1,3,3,3,3,6,6,6,6,10,10,10,10,15,15,15,15
            , 21,21,21,21,28,28,28,28,36,36,36,36,45,45,45,45
            ]

accum_t11 :: [Int]
accum_t11 = take 40 $ embed (repeatedly 1.0 1
                             >>> accumBy (+) 0
                             >>> accumBy (+) 0
                             >>> dHold 0)
                            accum_inp5

accum_t11r :: [Int]
accum_t11r = [ 0,0,0,0,0,1,1,1,1,3,3,3,3,6,6,6,6,10,10,10,10,15,15,15
             , 15,21,21,21,21,28,28,28,28,36,36,36,36,45,45,45
             ]

accum_t10 :: [Int]
accum_t10 = take 40 $ embed (repeatedly 1.0 1
                             >>> accumBy (+) 0
                             >>> accumHoldBy (+) 0)
                            accum_inp5

accum_t10r :: [Int]
accum_t10r = accum_t9 -- Should agree!

accum_t12 :: [Int]
accum_t12 = take 40 $ embed (repeatedly 1.0 1
                             >>> accumBy (+) 0
                             >>> dAccumHoldBy (+) 0)
                            accum_inp5

accum_t12r :: [Int]
accum_t12r = accum_t11 -- Should agree!

accum_t4 :: [Event (Bool,Int)]
accum_t4 = take 16 $ embed (accumFilter accum_accFiltFun1 0) accum_inp3

accum_t4r :: [Event (Bool,Int)]
accum_t4r =
  [ NoEvent,         NoEvent, NoEvent,         NoEvent
  , NoEvent,         NoEvent, NoEvent,         NoEvent
  , Event (False,6), NoEvent, Event (True,16), NoEvent
  , Event (True,16), NoEvent, NoEvent,         NoEvent
  ]

accum_accFiltFun1 a d =
  let a' = a + floor d
  in if even a'
       then (a', Just (a' > 10, a'))
       else (a', Nothing)

accum_t5 :: [Event (Bool,Int)]
accum_t5 = take 16 $ embed (accumFilter accum_accFiltFun2 0) accum_inp4

accum_t5r :: [Event (Bool,Int)]
accum_t5r =
  [ Event (False,1), NoEvent,         NoEvent, NoEvent
  , Event (False,3), NoEvent,         NoEvent, NoEvent
  , NoEvent,         Event (True,11), NoEvent, NoEvent
  , NoEvent,         NoEvent,         NoEvent, NoEvent
  ]

accum_accFiltFun2 a d =
  let a' = a + floor d
  in if odd a'
       then (a', Just (a' > 10, a'))
       else (a', Nothing)

-- This can be seen as the definition of accumFilter
accumFilter2 :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter2 f c_init =
    switch (never &&& attach c_init) afAux
  where
    afAux (c, a) =
      case f c a of
        (c', Nothing) -> switch (never &&& (notYet>>>attach c')) afAux
        (c', Just b)  -> switch (now b &&& (notYet>>>attach c')) afAux

    attach :: b -> SF (Event a) (Event (b, a))
    attach c = arr (fmap (\a -> (c, a)))

accum_t6 :: [Event (Bool,Int)]
accum_t6 = take 16 $ embed (accumFilter2 accum_accFiltFun1 0) accum_inp3

accum_t6r = accum_t4 -- Should agree!

accum_t7 :: [Event (Bool,Int)]
accum_t7 = take 16 $ embed (accumFilter2 accum_accFiltFun2 0) accum_inp4

accum_t7r = accum_t5 -- Should agree!

accum_accFiltFun3 :: Int -> Int -> (Int, Maybe Int)
accum_accFiltFun3 s a =
  let s' = s + a
  in if odd s'
       then (s', Just s')
       else (s', Nothing)

accum_t13 :: [Event Int]
accum_t13 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumFilter accum_accFiltFun3 0
                            >>> accumBy (+) 0
                            >>> accumBy (+) 0)
                            accum_inp5

accum_t13r :: [Event Int]
accum_t13r = [ NoEvent,  NoEvent, NoEvent, NoEvent
             , Event 1,  NoEvent, NoEvent, NoEvent
             , NoEvent,  NoEvent, NoEvent, NoEvent
             , Event 5,  NoEvent, NoEvent, NoEvent
             , NoEvent,  NoEvent, NoEvent, NoEvent
             , Event 14, NoEvent, NoEvent, NoEvent
             , NoEvent,  NoEvent, NoEvent, NoEvent
             , Event 30, NoEvent, NoEvent, NoEvent
             , NoEvent,  NoEvent, NoEvent, NoEvent
             , Event 55, NoEvent, NoEvent, NoEvent
             ]

accum_t14 :: [Int]
accum_t14 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumFilter accum_accFiltFun3 0
                            >>> accumBy (+) 0
                            >>> accumBy (+) 0
                            >>> hold 0)
                            accum_inp5

accum_t14r :: [Int]
accum_t14r = [ 0,0,0,0,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,5,14,14,14,14
             , 14,14,14,14,30,30,30,30,30,30,30,30,55,55,55,55
             ]

accum_t15 :: [Int]
accum_t15 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumFilter accum_accFiltFun3 0
                            >>> accumBy (+) 0
                            >>> accumHoldBy (+) 0)
                            accum_inp5

accum_t15r :: [Int]
accum_t15r = accum_t14 -- Should agree!

accum_t16 :: [Int]
accum_t16 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumFilter accum_accFiltFun3 0
                            >>> accumBy (+) 0
                            >>> accumBy (+) 0
                            >>> dHold 0)
                            accum_inp5

accum_t16r :: [Int]
accum_t16r = [ 0,0,0,0,0,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,5,14,14,14
             , 14,14,14,14,14,30,30,30,30,30,30,30,30,55,55,55
             ]

accum_t17 :: [Int]
accum_t17 = take 40 $ embed (repeatedly 1.0 1
                            >>> accumFilter accum_accFiltFun3 0
                            >>> accumBy (+) 0
                            >>> dAccumHoldBy (+) 0)
                            accum_inp5

accum_t17r :: [Int]
accum_t17r = accum_t16 -- Should agree!
