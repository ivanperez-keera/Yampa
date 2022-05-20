{-# LANGUAGE Arrows #-}
-- |
-- Description : Test cases for FRP.Yampa.Switches
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Switches
    ( tests
    )
  where

import Data.Fixed
import Data.List (findIndex)
import Data.Maybe (fromJust)

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
  [ testProperty "parB (fixed)"        (property $ coc_t0 ~= coc_t0r)
  , testProperty "parB (qc)"           prop_broadcast
  , testProperty "switch (0, fixed)"   (property $ switch_t0 ~= switch_t0r)
  , testProperty "switch (1, fixed)"   (property $ switch_t1 ~= switch_t1r)
  , testProperty "switch (2, fixed)"   (property $ switch_t2 ~= switch_t2r)
  , testProperty "switch (3, fixed)"   (property $ switch_t3 ~= switch_t3r)
  , testProperty "switch (4, fixed)"   (property $ switch_t4 ~= switch_t4r)
  , testProperty "switch (5, fixed)"   (property $ switch_t5 ~= switch_t5r)
  , testProperty "kswitch (0, fixed)"  (property $ kswitch_t0 ~= kswitch_t0r)
  , testProperty "kswitch (1, fixed)"  (property $ kswitch_t1 ~= kswitch_t1r)
  , testProperty "kswitch (2, fixed)"  (property $ kswitch_t2 ~= kswitch_t2r)
  , testProperty "kswitch (3, fixed)"  (property $ kswitch_t3 ~= kswitch_t3r)
  , testProperty "kswitch (4, fixed)"  (property $ kswitch_t4 ~= kswitch_t4r)
  , testProperty "pswitch (0, fixed)"  (property $ pswitch_t0 ~= pswitch_t0r)
  , testProperty "pswitch (1, fixed)"  (property $ pswitch_t1 ~= pswitch_t1r)
  , testProperty "pswitch (2, fixed)"  (property $ pswitch_t2 ~= pswitch_t2r)
  , testProperty "pswitch (3, fixed)"  (property $ pswitch_t3 ~= pswitch_t3r)
  , testProperty "pswitch (4, fixed)"  (property $ pswitch_t4 ~= pswitch_t4r)
  , testProperty "pswitch (5, fixed)"  (property $ pswitch_t5 ~= pswitch_t5r)
  , testProperty "rpswitch (0, fixed)" (property $ rpswitch_t0 ~= rpswitch_t0r)
  , testProperty "rpswitch (1, fixed)" (property $ rpswitch_t1 ~= rpswitch_t1r)
  , testProperty "rpswitch (2, fixed)" (property $ rpswitch_t2 ~= rpswitch_t2r)
  , testProperty "rpswitch (3, fixed)" (property $ rpswitch_t3 ~= rpswitch_t3r)
  , testProperty "rpswitch (4, fixed)" (property $ rpswitch_t4 ~= rpswitch_t4r)
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

prop_switch_t1 =
    forAll myStream $ evalT $
      Always $ SP ((switch_t1rec 42.0 &&& switch_tr) >>> arr same)

  where myStream :: Gen (SignalSampleStream Double)
        myStream = fixedDelayStreamWith f 1.0
        f dt = l!!(floor dt)
        l = [ 1.0, 1.0, 1.0
            , 2.0
            , 3.0, 3.0
            , 4.0, 4.0, 4.0
            , 5.0
            , 6.0, 6.0
            , 7.0, 7.0, 7.0
            , 8.0
            ]
             ++ repeat 9.0

        same = (uncurry (==))

switch_tr :: SF Double (Double, Double, Double)
switch_tr = proc (a) -> do
  t <- localTime -< ()
  let mt = fromIntegral $ floor (mod' t 4.0)
      v  = case floor (t / 4.0) of
             0 -> 42.0
             1 -> 3.0
             2 -> 4.0
             3 -> 7.0
             _ -> 9.0
  returnA -< (a, mt, v)

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

-- * Test cases for pSwitchB and dpSwitchB

pswitch_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

whenFstGE :: Ord a => a -> c -> SF (a, b) (Event c)
whenFstGE a c = arr fst >>> arr (>= a) >>> edge >>> arr (`tag` c)

pswitch_t0 :: [[Double]]
pswitch_t0 = take 20 $ embed sf pswitch_inp1
  where
    sf =
      pSwitchB [] (whenFstGE 1.25 10.0) $ \sfs x ->
      pSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
      pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0) $ \sfs x->
      pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 7.25 20.0) $ \sfs _->
      parB (take 2 sfs)

pswitch_t0r =
  [ []                    -- 0.0
  , []                    -- 0.5
  , []                    -- 1.0
  , [0.0]                 -- 1.5
  , [0.15]                -- 2.0
  , [0.35]                -- 2.5
  , [0.60]                -- 3.0
  , [0.90]                -- 3.5
  , [10.00, 1.25]         -- 4.0
  , [10.40, 1.65]         -- 4.5
  , [10.85, 2.10]         -- 5.0
  , [20.00, 11.35, 2.60]  -- 5.5
  , [20.55, 11.90, 3.15]  -- 6.0
  , [21.15, 12.50, 3.75]  -- 6.5
  , [21.80, 13.15, 4.40]  -- 7.0
  , [22.50, 13.85]        -- 7.5
  , [23.25, 14.60]        -- 8.0
  , [24.05, 15.40]        -- 8.5
  , [24.90, 16.25]        -- 9.0
  , [25.80, 17.15]        -- 9.5
  ]

pswitch_t1 :: [[Double]]
pswitch_t1 = take 20 $ embed sf pswitch_inp1
  where
    sf =
      dpSwitchB [] (whenFstGE 1.25 10.0) $ \sfs x ->
      dpSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
      dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0)$ \sfs x->
      dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 7.25 20.0)$ \sfs _->
      parB (take 2 sfs)

pswitch_t1r =
  [ []                    -- 0.0
  , []                    -- 0.5
  , []                    -- 1.0
  , []                    -- 1.5
  , [0.15]                -- 2.0
  , [0.35]                -- 2.5
  , [0.60]                -- 3.0
  , [0.90]                -- 3.5
  , [1.25]                -- 4.0
  , [10.40, 1.65]         -- 4.5
  , [10.85, 2.10]         -- 5.0
  , [11.35, 2.60]         -- 5.5
  , [20.55, 11.90, 3.15]  -- 6.0
  , [21.15, 12.50, 3.75]  -- 6.5
  , [21.80, 13.15, 4.40]  -- 7.0
  , [22.50, 13.85, 5.10]  -- 7.5
  , [23.25, 14.60]        -- 8.0
  , [24.05, 15.40]        -- 8.5
  , [24.90, 16.25]        -- 9.0
  , [25.80, 17.15]        -- 9.5
  ]

pswitch_t2 :: [[Double]]
pswitch_t2 = take 20 $ embed sf pswitch_inp1
  where
    sf =
      pSwitchB [] (now 10.0) $ \sfs x ->
      pSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
      pSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0) $ \sfs x->
      pSwitchB ((integral>>>arr(+x)):sfs)(now 20.0) $ \sfs _->
      parB (take 2 sfs)

pswitch_t2r =
  [ [0.00]          -- 0.0
  , [0.00]          -- 0.5
  , [0.05]          -- 1.0
  , [0.15]          -- 1.5
  , [0.30]          -- 2.0
  , [0.50]          -- 2.5
  , [0.75]          -- 3.0
  , [1.05]          -- 3.5
  , [10.00,  1.40]  -- 4.0
  , [10.40,  1.80]  -- 4.5
  , [10.85,  2.25]  -- 5.0
  , [20.00, 11.35]  -- 5.5
  , [20.55, 11.90]  -- 6.0
  , [21.15, 12.50]  -- 6.5
  , [21.80, 13.15]  -- 7.0
  , [22.50, 13.85]  -- 7.5
  , [23.25, 14.60]  -- 8.0
  , [24.05, 15.40]  -- 8.5
  , [24.90, 16.25]  -- 9.0
  , [25.80, 17.15]  -- 9.5
  ]

pswitch_t3 :: [[Double]]
pswitch_t3 = take 20 $ embed sf pswitch_inp1
  where
    sf =
      dpSwitchB [] (now 10.0) $ \sfs x ->
      dpSwitchB (integral:sfs) (whenFstGE 3.75 10.0) $ \sfs x ->
      dpSwitchB ((integral>>>arr(+x)):sfs)(whenFstGE 5.25 20.0)$ \sfs x->
      dpSwitchB ((integral>>>arr(+x)):sfs) (now 20.0) $ \sfs _->
      parB (take 2 sfs)

pswitch_t3r =
  [ []                -- 0.0
  , [0.00]            -- 0.5
  , [0.05]            -- 1.0
  , [0.15]            -- 1.5
  , [0.30]            -- 2.0
  , [0.50]            -- 2.5
  , [0.75]            -- 3.0
  , [1.05]            -- 3.5
  , [1.40]            -- 4.0
  , [10.40,  1.80]    -- 4.5
  , [10.85,  2.25]    -- 5.0
  , [11.35,  2.75]    -- 5.5
  , [20.55, 11.90]    -- 6.0
  , [21.15, 12.50]    -- 6.5
  , [21.80, 13.15]    -- 7.0
  , [22.50, 13.85]    -- 7.5
  , [23.25, 14.60]    -- 8.0
  , [24.05, 15.40]    -- 8.5
  , [24.90, 16.25]    -- 9.0
  , [25.80, 17.15]    -- 9.5
  ]

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The
-- observaton of the output is done via the loop (rather than the directly
-- from the outputs of the signal functions in the collection), thus the
-- use of a delayed switch is essential.

pswitch_ramp :: Double -> SF a Double
pswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit :: Double -> SF ((a, [Double]), b) (Event Int)
pswitch_limit x = arr (snd . fst) >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t4 :: [[Double]]
pswitch_t4 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = dpSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                   (pswitch_limit 2.99)
                   pswitch_t4rec
         >>> arr dup

pswitch_t4rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t4rec sfs n =
  dpSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
            (pswitch_limit 2.99)
            pswitch_t4rec

pswitch_t4r =
  [ [0.0, 1.0, 2.0]
  , [0.2, 1.2, 2.2]
  , [0.4, 1.4, 2.4]
  , [0.6, 1.6, 2.6]
  , [0.8, 1.8, 2.8]
  , [1.0, 2.0, 3.0]
  , [1.2, 2.2, 0.2]
  , [1.4, 2.4, 0.4]
  , [1.6, 2.6, 0.6]
  , [1.8, 2.8, 0.8]
  , [2.0, 3.0, 1.0]
  , [2.2, 0.2, 1.2]
  , [2.4, 0.4, 1.4]
  , [2.6, 0.6, 1.6]
  , [2.8, 0.8, 1.8]
  , [3.0, 1.0, 2.0]
  , [0.2, 1.2, 2.2]
  , [0.4, 1.4, 2.4]
  , [0.6, 1.6, 2.6]
  , [0.8, 1.8, 2.8]
  , [1.0, 2.0, 3.0]
  , [1.2, 2.2, 0.2]
  , [1.4, 2.4, 0.4]
  , [1.6, 2.6, 0.6]
  , [1.8, 2.8, 0.8]
  , [2.0, 3.0, 1.0]
  , [2.2, 0.2, 1.2]
  , [2.4, 0.4, 1.4]
  , [2.6, 0.6, 1.6]
  , [2.8, 0.8, 1.8]
  ]

-- Variation of the test above, with direct observation (not via loop) and
-- immediate switch.

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit2 :: Double -> SF (a, [Double]) (Event Int)
pswitch_limit2 x = arr snd >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t5 :: [([Double], Double)]
pswitch_t5 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
  where
    sf :: SF (a, [Double]) (([Double], Double), [Double])
    sf = ((pSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                    (pswitch_limit2 2.99)
                    pswitch_t5rec)
          &&& (arr snd >>> arr sum))
         >>> arr (\(xs, y) -> ((xs, y), xs))

pswitch_t5rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t5rec sfs n =
  pSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
           (pswitch_limit2 2.99)
           pswitch_t5rec

pswitch_t5r =
  [ ([0.0, 1.0, 2.0], 3.0)
  , ([0.2, 1.2, 2.2], 3.6)
  , ([0.4, 1.4, 2.4], 4.2)
  , ([0.6, 1.6, 2.6], 4.8)
  , ([0.8, 1.8, 2.8], 5.4)
  , ([1.0, 2.0, 0.0], 3.0)
  , ([1.2, 2.2, 0.2], 3.6)
  , ([1.4, 2.4, 0.4], 4.2)
  , ([1.6, 2.6, 0.6], 4.8)
  , ([1.8, 2.8, 0.8], 5.4)
  , ([2.0, 0.0, 1.0], 3.0)
  , ([2.2, 0.2, 1.2], 3.6)
  , ([2.4, 0.4, 1.4], 4.2)
  , ([2.6, 0.6, 1.6], 4.8)
  , ([2.8, 0.8, 1.8], 5.4)
  , ([0.0, 1.0, 2.0], 3.0)
  , ([0.2, 1.2, 2.2], 3.6)
  , ([0.4, 1.4, 2.4], 4.2)
  , ([0.6, 1.6, 2.6], 4.8)
  , ([0.8, 1.8, 2.8], 5.4)
  , ([1.0, 2.0, 0.0], 3.0)
  , ([1.2, 2.2, 0.2], 3.6)
  , ([1.4, 2.4, 0.4], 4.2)
  , ([1.6, 2.6, 0.6], 4.8)
  , ([1.8, 2.8, 0.8], 5.4)
  , ([2.0, 0.0, 1.0], 3.0)
  , ([2.2, 0.2, 1.2], 3.6)
  , ([2.4, 0.4, 1.4], 4.2)
  , ([2.6, 0.6, 1.6], 4.8)
  , ([2.8, 0.8, 1.8], 5.4)
  ]

-- * Test cases for rpSwitchB and drpSwitchB

rpswitch_inp1 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
  where
    delta_inp =
      [ Just (1.0, NoEvent), Nothing, Nothing
      , Just (2.0, Event (integral:)), Just (3.0, NoEvent), Nothing
      , Just (4.0, NoEvent), Nothing, Nothing
      , Just (5.0, Event ((integral >>> arr (+100.0)):))
      , Just (6.0, NoEvent), Nothing
      , Just (7.0, NoEvent), Nothing, Nothing
      , Just (8.0, Event tail), Just (9.0, NoEvent), Nothing
      ]
      ++ repeat Nothing

-- This input contains exaples of "continuos switching", i.e. the same
-- switching event ocurring during a a few contiguous time steps.
-- It also starts with an immediate switch.
rpswitch_inp2 = (fromJust (head delta_inp), zip (repeat 1.0) (tail delta_inp))
  where
    delta_inp =
      [ Just (1.0, Event (integral:))
      , Just (1.0, NoEvent), Nothing
      , Just (2.0, Event ((integral >>> arr(+100.0)):)), Nothing, Nothing
      , Just (3.0, Event ((integral >>> arr(+200.0)):)), Nothing, Nothing
      , Just (4.0, NoEvent), Nothing, Nothing
      , Just (5.0, Event ((arr (*3)):))
      , Just (5.0, NoEvent), Nothing
      , Just (6.0, Event tail), Just (7.0, Event ((arr (*7)):))
      , Just (8.0, Event (take 2))
      , Just (9.0, NoEvent), Nothing
      ]
      ++ repeat Nothing

rpswitch_t0 :: [[Double]]
rpswitch_t0 = take 20 $ embed (rpSwitchB []) rpswitch_inp1

rpswitch_t0r =
  [ []             -- 0 s
  , []             -- 1 s
  , []             -- 2 s
  , [0.0]          -- 3 s
  , [2.0]          -- 4 s
  , [5.0]          -- 5 s
  , [8.0]          -- 6 s
  , [12.0]         -- 7 s
  , [16.0]         -- 8 s
  , [100.0, 20.0]  -- 9 s
  , [105.0, 25.0]  -- 10 s
  , [111.0, 31.0]  -- 11 s
  , [117.0, 37.0]  -- 12 s
  , [124.0, 44.0]  -- 13 s
  , [131.0, 51.0]  -- 14 s
  , [58.0]         -- 15 s
  , [66.0]         -- 16 s
  , [75.0]         -- 17 s
  , [84.0]         -- 18 s
  , [93.0]         -- 19 s
  ]

rpswitch_t1 :: [[Double]]
rpswitch_t1 = take 20 $ embed (drpSwitchB []) rpswitch_inp1

rpswitch_t1r =
  [ []              -- 0 s
  , []              -- 1 s
  , []              -- 2 s
  , []              -- 3 s
  , [2.0]           -- 4 s
  , [5.0]           -- 5 s
  , [8.0]           -- 6 s
  , [12.0]          -- 7 s
  , [16.0]          -- 8 s
  , [20.0]          -- 9 s
  , [105.0, 25.0]   -- 10 s
  , [111.0, 31.0]   -- 11 s
  , [117.0, 37.0]   -- 12 s
  , [124.0, 44.0]   -- 13 s
  , [131.0, 51.0]   -- 14 s
  , [138.0, 58.0]   -- 15 s
  , [66.0]          -- 16 s
  , [75.0]          -- 17 s
  , [84.0]          -- 18 s
  , [93.0]          -- 19 s
  ]

rpswitch_t2 :: [[Double]]
rpswitch_t2 = take 20 $ embed (rpSwitchB []) rpswitch_inp2

rpswitch_t2r =
  [ [0.0]                                                   -- 0 s
  , [1.0]                                                   -- 1 s
  , [2.0]                                                   -- 2 s
  , [100.0, 3.0]                                            -- 3 s
  , [100.0, 102.0, 5.0]                                     -- 4 s
  , [100.0, 102.0, 104.0, 7.0]                              -- 5 s
  , [200.0, 102.0, 104.0, 106.0, 9.0]                       -- 6 s
  , [200.0, 203.0, 105.0, 107.0, 109.0, 12.0]               -- 7 s
  , [200.0, 203.0, 206.0, 108.0, 110.0, 112.0, 15.0]        -- 8 s
  , [203.0, 206.0, 209.0, 111.0, 113.0, 115.0, 18.0]        -- 9 s
  , [207.0, 210.0, 213.0, 115.0, 117.0, 119.0, 22.0]        -- 10 s
  , [211.0, 214.0, 217.0, 119.0, 121.0, 123.0, 26.0]        -- 11 s
  , [15.0, 215.0, 218.0, 221.0, 123.0, 125.0, 127.0, 30.0]  -- 12 s
  , [15.0, 220.0, 223.0, 226.0, 128.0, 130.0, 132.0, 35.0]  -- 13 s
  , [15.0, 225.0, 228.0, 231.0, 133.0, 135.0, 137.0, 40.0]  -- 14 s
  , [230.0, 233.0, 236.0, 138.0, 140.0, 142.0, 45.0]        -- 15 s
  , [49.0, 236.0, 239.0, 242.0, 144.0, 146.0, 148.0, 51.0]  -- 16 s
  , [56.0, 243.0]                                           -- 17 s
  , [63.0, 251.0]                                           -- 18 s
  , [63.0, 260.0]                                           -- 19 s
  ]

rpswitch_t3 :: [[Double]]
rpswitch_t3 = take 20 $ embed (drpSwitchB []) rpswitch_inp2

rpswitch_t3r =
  [ []                                                      -- 0 s
  , [1.0]                                                   -- 1 s
  , [2.0]                                                   -- 2 s
  , [3.0]                                                   -- 3 s
  , [102.0, 5.0]                                            -- 4 s
  , [102.0, 104.0, 7.0]                                     -- 5 s
  , [102.0, 104.0, 106.0, 9.0]                              -- 6 s
  , [203.0, 105.0, 107.0, 109.0, 12.0]                      -- 7 s
  , [203.0, 206.0, 108.0, 110.0, 112.0, 15.0]               -- 8 s
  , [203.0, 206.0, 209.0, 111.0, 113.0, 115.0, 18.0]        -- 9 s
  , [207.0, 210.0, 213.0, 115.0, 117.0, 119.0, 22.0]        -- 10 s
  , [211.0, 214.0, 217.0, 119.0, 121.0, 123.0, 26.0]        -- 11 s
  , [215.0, 218.0, 221.0, 123.0, 125.0, 127.0, 30.0]        -- 12 s
  , [15.0, 220.0, 223.0, 226.0, 128.0, 130.0, 132.0, 35.0]  -- 13 s
  , [15.0, 225.0, 228.0, 231.0, 133.0, 135.0, 137.0, 40.0]  -- 14 s
  , [18.0, 230.0, 233.0, 236.0, 138.0, 140.0, 142.0, 45.0]  -- 15 s
  , [236.0, 239.0, 242.0, 144.0, 146.0, 148.0, 51.0]        -- 16 s
  , [56.0, 243.0, 246.0, 249.0, 151.0, 153.0, 155.0, 58.0]  -- 17 s
  , [63.0, 251.0]                                           -- 18 s
  , [63.0, 260.0]                                           -- 19 s
  ]

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The observaton
-- of the output is done via a loop, thus the  use of a delayed switch is
-- essential.

rpswitch_ramp :: Double -> SF a Double
rpswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
rpswitch_limit :: Double -> SF [Double] (Event ([SF a Double]->[SF a Double]))
rpswitch_limit x = arr (findIndex (>=x)) >>> edgeJust >>> arr (fmap restart)
  where
    restart n = \sfs -> take n sfs ++ [rpswitch_ramp 0.0] ++ drop (n+1) sfs

rpswitch_t4 :: [[Double]]
rpswitch_t4 = take 30 $ embed (loop sf) (deltaEncode 0.1 (repeat ()))
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = (second (rpswitch_limit 2.99)
          >>> drpSwitchB [ rpswitch_ramp 0.0
                         , rpswitch_ramp 1.0
                         , rpswitch_ramp 2.0
                         ]
         )
         >>> arr dup

rpswitch_t4r =
  [ [0.0, 1.0, 2.0]
  , [0.2, 1.2, 2.2]
  , [0.4, 1.4, 2.4]
  , [0.6, 1.6, 2.6]
  , [0.8, 1.8, 2.8]
  , [1.0, 2.0, 3.0]
  , [1.2, 2.2, 0.2]
  , [1.4, 2.4, 0.4]
  , [1.6, 2.6, 0.6]
  , [1.8, 2.8, 0.8]
  , [2.0, 3.0, 1.0]
  , [2.2, 0.2, 1.2]
  , [2.4, 0.4, 1.4]
  , [2.6, 0.6, 1.6]
  , [2.8, 0.8, 1.8]
  , [3.0, 1.0, 2.0]
  , [0.2, 1.2, 2.2]
  , [0.4, 1.4, 2.4]
  , [0.6, 1.6, 2.6]
  , [0.8, 1.8, 2.8]
  , [1.0, 2.0, 3.0]
  , [1.2, 2.2, 0.2]
  , [1.4, 2.4, 0.4]
  , [1.6, 2.6, 0.6]
  , [1.8, 2.8, 0.8]
  , [2.0, 3.0, 1.0]
  , [2.2, 0.2, 1.2]
  , [2.4, 0.4, 1.4]
  , [2.6, 0.6, 1.6]
  , [2.8, 0.8, 1.8]
  ]

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)
