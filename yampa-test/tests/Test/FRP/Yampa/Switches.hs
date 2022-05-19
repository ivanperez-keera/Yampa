-- |
-- Description : Test cases for FRP.Yampa.Switches
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Switches
    ( tests
    )
  where

import Data.Fixed

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.EventS (snap)
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Switches"
  [ testProperty "parB (fixed)"       (property $ coc_t0 ~= coc_t0r)
  , testProperty "parB (qc)"          prop_broadcast
  , testProperty "switch (0, fixed)"  (property $ switch_t0 ~= switch_t0r)
  , testProperty "switch (1, fixed)"  (property $ switch_t1 ~= switch_t1r)
  , testProperty "switch (2, fixed)"  (property $ switch_t2 ~= switch_t2r)
  , testProperty "switch (3, fixed)"  (property $ switch_t3 ~= switch_t3r)
  , testProperty "switch (4, fixed)"  (property $ switch_t4 ~= switch_t4r)
  , testProperty "switch (5, fixed)"  (property $ switch_t5 ~= switch_t5r)
  , testProperty "kswitch (0, fixed)" (property $ kswitch_t0 ~= kswitch_t0r)
  , testProperty "kswitch (1, fixed)" (property $ kswitch_t1 ~= kswitch_t1r)
  , testProperty "kswitch (2, fixed)" (property $ kswitch_t2 ~= kswitch_t2r)
  , testProperty "kswitch (3, fixed)" (property $ kswitch_t3 ~= kswitch_t3r)
  , testProperty "kswitch (4, fixed)" (property $ kswitch_t4 ~= kswitch_t4r)
  ]

-- * Test cases for collection-oriented combinators

coc_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

coc_t0 :: [[Double]]
coc_t0 = take 20 $ embed (parB [constant 1.0, identity, integral]) coc_inp1

coc_t0r =
  [ [1.0, 0.0, 0.00]
  , [1.0, 0.5, 0.00]
  , [1.0, 1.0, 0.05]
  , [1.0, 1.5, 0.15]
  , [1.0, 2.0, 0.30]
  , [1.0, 2.5, 0.50]
  , [1.0, 3.0, 0.75]
  , [1.0, 3.5, 1.05]
  , [1.0, 4.0, 1.40]
  , [1.0, 4.5, 1.80]
  , [1.0, 5.0, 2.25]
  , [1.0, 5.5, 2.75]
  , [1.0, 6.0, 3.30]
  , [1.0, 6.5, 3.90]
  , [1.0, 7.0, 4.55]
  , [1.0, 7.5, 5.25]
  , [1.0, 8.0, 6.00]
  , [1.0, 8.5, 6.80]
  , [1.0, 9.0, 7.65]
  , [1.0, 9.5, 8.55]
  ]

-- Par with broadcast (collection-oriented combinators)
-- TODO: Add integral to the list of SFs being tested
prop_broadcast =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = parB [identity, (arr (+1))]
        pred = (\x [y,z] -> x == y && (x + 1) == z)

-- * Test cases for switch and dSwitch

switch_inp1 = deltaEncode 1.0 $
  [ 1.0, 1.0, 1.0
  , 2.0
  , 3.0, 3.0
  , 4.0, 4.0, 4.0
  , 5.0
  , 6.0, 6.0
  , 7.0, 7.0, 7.0
  , 8.0
  ]
   ++ repeat 9.0

switch_t0 = take 18 $
  embed (switch switch_t0a $ \x ->
         switch (switch_t0b x) $ \x ->
         switch (switch_t0c x) $ \x ->
         switch (switch_t0c x) $ \x ->
         switch (switch_t0d x) $ \x ->
         switch (switch_t0e x) $ \x ->
         switch (switch_t0e x) $
         switch_t0final)
        switch_inp1

switch_t0a :: SF Double (Double, Event Int)
switch_t0a = localTime
             >>> arr dup
             >>> second (arr (>= 3.0) >>> edge >>> arr (`tag` 17))

switch_t0b :: Int -> SF Double (Double, Event Int)
switch_t0b x = localTime
               >>> arr dup
               >>> second (arr (>= 3.0) >>> edge >>> arr (`tag` (23 + x)))

-- This should raise an event IMMEDIATELY: no time should pass.
switch_t0c :: Num b => b -> SF a (a, Event b)
switch_t0c x = arr dup >>> second (now (x + 1))

switch_t0d x = (arr (+ (fromIntegral x))) &&& (arr (>= 7.0) >>> edge)

-- This should raise an event IMMEDIATELY: no time should pass.
switch_t0e :: b -> SF a (a, Event a)
switch_t0e _ = arr dup >>> second snap

switch_t0final :: Double -> SF Double Double
switch_t0final x = arr (+x)

switch_t0r =
  [ 0.0,  1.0,  2.0                     -- switch_t0a
  , 0.0,  1.0,  2.0                     -- switch_t0b
  , 46.0, 46.0, 46.0, 47.0, 48.0, 48.0  -- switch_t0d
  , 14.0, 14.0, 14.0, 15.0, 16.0, 16.0  -- switch_t0final
  ]

switch_t1 = take 32 $ embed (switch_t1rec 42.0) switch_inp1

-- Outputs current input, local time, and the value of the initializing
-- argument until some time has passed (determined by integrating a constant),
-- at which point an event occurs.
switch_t1a :: Double -> SF Double ((Double,Double,Double), Event ())
switch_t1a x = (arr dup >>> second localTime >>> arr (\(a,t) -> (a,t,x)))
               &&& (constant 0.5
                    >>> integral
                    >>> (arr (>= (2.0 :: Double)) -- Used to work with no sig.
                    >>> edge))

-- This should raise an event IMMEDIATELY: no time should pass.
switch_t1b :: b -> SF a ((Double,Double,Double), Event a)
switch_t1b _ = constant (-999.0,-999.0,-999.0) &&& snap

switch_t1rec :: Double -> SF Double (Double,Double,Double)
switch_t1rec x =
  switch (switch_t1a x) $ \x ->
  switch (switch_t1b x) $ \x ->
  switch (switch_t1b x) $
  switch_t1rec

switch_t1r =
  [ (1.0,0.0,42.0), (1.0,1.0,42.0), (1.0,2.0,42.0), (2.0,3.0,42.0)
  , (3.0,0.0,3.0),  (3.0,1.0,3.0),  (4.0,2.0,3.0),  (4.0,3.0,3.0)
  , (4.0,0.0,4.0),  (5.0,1.0,4.0),  (6.0,2.0,4.0),  (6.0,3.0,4.0)
  , (7.0,0.0,7.0),  (7.0,1.0,7.0),  (7.0,2.0,7.0),  (8.0,3.0,7.0)
  , (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  ]

switch_t2 = take 18 $
  embed (dSwitch switch_t0a $ \x ->
         dSwitch (switch_t0b x) $ \x ->
         dSwitch (switch_t0c x) $ \x ->
         dSwitch (switch_t0c x) $ \x ->
         dSwitch (switch_t0d x) $ \x ->
         dSwitch (switch_t0e x) $ \x ->
         dSwitch (switch_t0e x) $
         switch_t0final)
        switch_inp1

switch_t2r =
  [ 0.0,  1.0,  2.0                     -- switch_t0a
  , 3.0,  1.0,  2.0                     -- switch_t0b
  , 3.0,  46.0, 46.0, 47.0, 48.0, 48.0  -- switch_t0d
  , 49.0, 14.0, 14.0, 15.0, 16.0, 16.0  -- switch_t0final
  ]

switch_t3 = take 32 $ embed (switch_t3rec 42.0) switch_inp1

switch_t3rec :: Double -> SF Double (Double,Double,Double)
switch_t3rec x =
  dSwitch (switch_t1a x) $ \x ->
  dSwitch (switch_t1b x) $ \x ->
  dSwitch (switch_t1b x) $
  switch_t3rec

switch_t3r =
  [ (1.0,0.0,42.0), (1.0,1.0,42.0), (1.0,2.0,42.0), (2.0,3.0,42.0)
  , (3.0,4.0,42.0), (3.0,1.0,3.0),  (4.0,2.0,3.0),  (4.0,3.0,3.0)
  , (4.0,4.0,3.0),  (5.0,1.0,4.0),  (6.0,2.0,4.0),  (6.0,3.0,4.0)
  , (7.0,4.0,4.0),  (7.0,1.0,7.0),  (7.0,2.0,7.0),  (8.0,3.0,7.0)
  , (9.0,4.0,7.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  , (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)
  ]

-- The correct strictness properties of dSwitch are crucial here.
-- switch does not work.
switch_t4 = take 25 $
  embed (loop $
           dSwitch switch_t4a $ \_ ->
           dSwitch switch_t4a $ \_ ->
           dSwitch switch_t4a $ \_ ->
           switch_t4final
         )
        (deltaEncode 1.0 (repeat ()))

switch_t4a :: SF (a, Double) ((Double, Double), Event ())
switch_t4a = (constant 1.0 >>> integral >>> arr dup)
             &&& (arr (\ (_, x) -> x >= 5.0) >>> edge)

switch_t4final :: SF (a, Double) (Double, Double)
switch_t4final = constant 0.1 >>> integral >>> arr dup

switch_t4r =
  [ 0.0, 1.0, 2.0, 3.0, 4.0                           -- switch_t4a
  , 5.0, 1.0, 2.0, 3.0, 4.0                           -- switch_t4a
  , 5.0, 1.0, 2.0, 3.0, 4.0                           -- switch_t4a
  , 5.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9  -- switch_t4final
  ]

impulseIntegral2 :: VectorSpace a s => SF (a, Event a) a
impulseIntegral2 =
   switch (first integral >>> arr (\(a, ea) -> (a, fmap (^+^ a) ea)))
          impulseIntegral2'
 where
   impulseIntegral2' :: VectorSpace a s => a -> SF (a, Event a) a
   impulseIntegral2' a =
     switch ((integral >>> arr (^+^ a)) *** notYet
             >>> arr (\(a, ea) -> (a, fmap (^+^ a) ea)))
            impulseIntegral2'

switch_t5 :: [Double]
switch_t5 = take 50 $ embed impulseIntegral2
                            (deltaEncode 0.1 (zip (repeat 1.0) evSeq))
  where
    evSeq = replicate 9 NoEvent ++ [Event 10.0]
            ++ replicate 9 NoEvent ++ [Event (-10.0)]
            ++ evSeq

switch_t5r =
  [  0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8, 10.9
  , 11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8,  1.9
  ,  2.0,  2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8, 12.9
  , 13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8,  3.9
  ,  4.0,  4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8, 14.9
  ]


-- * Test cases for kSwitch and dkSwitch

kswitch_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

whenSndGE :: Ord b => b -> c -> SF (a, b) (Event c)
whenSndGE b c = arr snd >>> arr (>= b) >>> edge >>> arr (`tag` c)

kswitch_t0 :: [Double]
kswitch_t0 = take 20 $ embed sf kswitch_inp1
  where
    sf =
      kSwitch integral (whenSndGE 0.2 (-1.0)) $ \sf1 x ->
      kSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
      sf1

kswitch_t0r =
  [  0.00,  0.00,  0.05, 0.15, -1.00
  , -0.80, -0.55, -0.25, 0.10,  0.50
  ,  0.95,  0.30,  0.85, 1.45,  2.10
  ,  2.80,  3.55,  4.35, 5.20,  6.10
  ]

kswitch_t1 :: [Double]
kswitch_t1 = take 20 $ embed sf kswitch_inp1
  where
    sf =
      dkSwitch integral (whenSndGE 0.2 (-1.0)) $ \sf1 x ->
      dkSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
      sf1

kswitch_t1r =
  [  0.00,  0.00,  0.05, 0.15, 0.30
  , -0.80, -0.55, -0.25, 0.10, 0.50
  ,  0.95,  1.45,  0.85, 1.45, 2.10
  ,  2.80,  3.55,  4.35, 5.20, 6.10
  ]

kswitch_t2 :: [Double]
kswitch_t2 = take 20 $ embed sf kswitch_inp1
  where
    sf =
      kSwitch integral (now (-1.0)) $ \sf1 x ->
      kSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
      sf1

kswitch_t2r =
  [ -1.00, -1.00, -0.95, -0.85, -0.70
  , -0.50, -0.25,  0.05,  0.40,  0.80
  ,  0.00,  0.50,  1.05,  1.65,  2.30
  ,  3.00,  3.75,  4.55,  5.40,  6.30
  ]

kswitch_t3 :: [Double]
kswitch_t3 = take 20 $ embed sf kswitch_inp1
  where
    sf =
      dkSwitch integral (now (-1.0)) $ \sf1 x ->
      dkSwitch (integral >>> arr (+x)) (whenSndGE 1.0 (1.0)) $ \_ _ ->
      sf1

kswitch_t3r =
  [  0.00, -1.00, -0.95, -0.85, -0.70
  , -0.50, -0.25,  0.05,  0.40,  0.80
  ,  1.25,  0.50,  1.05,  1.65,  2.30
  ,  3.00,  3.75,  4.55,  5.40,  6.30
  ]

-- The correct strictness properties of dkSwitch are crucial here.
-- kSwitch does not work.
kswitch_t4 = take 40 $
    embed (loop $
             dkSwitch sf (sfe 0.55 (-1.0))              $ \sf1 x ->
             dkSwitch (sf >>> arr2 (+x)) (sfe 0.05 8.0) $ \sf2 y ->
             dkSwitch sf1 (sfe 2.0 (-2.0))              $ \_   z ->
             sf2 >>> arr2 (+(y + z))
          )
          (deltaEncode 0.1 (repeat ()))
  where
    sf :: SF (a, Double) (Double, Double)
    sf = constant 1.0 >>> integral >>> arr dup

    sfe :: Double -> Double -> SF ((a, Double), b) (Event Double)
    sfe x e = arr fst >>> whenSndGE x e

    arr2 f = arr (\(x,y) -> (f x, f y))

kswitch_t4r =
  [  0.0,  0.1,  0.2,  0.3,  0.4
  ,  0.5,  0.6, -0.9, -0.8, -0.7
  , -0.6, -0.5, -0.4, -0.3, -0.2
  , -0.1,  0.0,  0.1,  0.7,  0.8
  ,  0.9,  1.0,  1.1,  1.2,  1.3
  ,  1.4,  1.5,  1.6,  1.7,  1.8
  ,  1.9,  2.0,  6.2,  6.3,  6.4
  ,  6.5,  6.6,  6.7,  6.8,  6.9
  ]

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)
