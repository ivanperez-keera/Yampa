{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsSwitch.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsSwitch					     *
*       Purpose:        Test cases for switch				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}
module AFRPTestsSwitch (switch_tr, switch_trs) where

import FRP.Yampa
import FRP.Yampa.EventS
import FRP.Yampa.Internals (Event(NoEvent, Event))

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for switch and dSwitch
------------------------------------------------------------------------------

switch_inp1 = deltaEncode 1.0 $
    [1.0, 1.0, 1.0,
     2.0,
     3.0, 3.0,
     4.0, 4.0, 4.0,
     5.0,
     6.0, 6.0,
     7.0, 7.0, 7.0, 
     8.0]
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
    [0.0,  1.0,  2.0, 				-- switch_t0a
     0.0,  1.0,  2.0,   			-- switch_t0b
     46.0, 46.0, 46.0, 47.0, 48.0, 48.0,	-- switch_t0d
     14.0, 14.0, 14.0, 15.0, 16.0, 16.0		-- switch_t0final
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
    [(1.0,0.0,42.0), (1.0,1.0,42.0), (1.0,2.0,42.0), (2.0,3.0,42.0),
     (3.0,0.0,3.0),  (3.0,1.0,3.0),  (4.0,2.0,3.0),  (4.0,3.0,3.0),
     (4.0,0.0,4.0),  (5.0,1.0,4.0),  (6.0,2.0,4.0),  (6.0,3.0,4.0),
     (7.0,0.0,7.0),  (7.0,1.0,7.0),  (7.0,2.0,7.0),  (8.0,3.0,7.0),
     (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,0.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)]

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
    [0.0,  1.0,  2.0,				-- switch_t0a
     3.0,  1.0,  2.0,				-- switch_t0b
     3.0,  46.0, 46.0, 47.0, 48.0, 48.0,	-- switch_t0d
     49.0, 14.0, 14.0, 15.0, 16.0, 16.0		-- switch_t0final
    ]


switch_t3 = take 32 $ embed (switch_t3rec 42.0) switch_inp1

switch_t3rec :: Double -> SF Double (Double,Double,Double)
switch_t3rec x =
    dSwitch (switch_t1a x) $ \x ->
    dSwitch (switch_t1b x) $ \x ->
    dSwitch (switch_t1b x) $
    switch_t3rec

switch_t3r =
    [(1.0,0.0,42.0), (1.0,1.0,42.0), (1.0,2.0,42.0), (2.0,3.0,42.0),
     (3.0,4.0,42.0), (3.0,1.0,3.0),  (4.0,2.0,3.0),  (4.0,3.0,3.0),
     (4.0,4.0,3.0),  (5.0,1.0,4.0),  (6.0,2.0,4.0),  (6.0,3.0,4.0),
     (7.0,4.0,4.0),  (7.0,1.0,7.0),  (7.0,2.0,7.0),  (8.0,3.0,7.0),
     (9.0,4.0,7.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0),
     (9.0,4.0,9.0),  (9.0,1.0,9.0),  (9.0,2.0,9.0),  (9.0,3.0,9.0)]


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
    [0.0, 1.0, 2.0, 3.0, 4.0,				-- switch_t4a
     5.0, 1.0, 2.0, 3.0, 4.0,				-- switch_t4a
     5.0, 1.0, 2.0, 3.0, 4.0,				-- switch_t4a
     5.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9	-- switch_t4final
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
    [ 0.0,  0.1,  0.2,  0.3,  0.4,  0.5,  0.6,  0.7,  0.8, 10.9,
     11.0, 11.1, 11.2, 11.3, 11.4, 11.5, 11.6, 11.7, 11.8,  1.9,
      2.0,  2.1,  2.2,  2.3,  2.4,  2.5,  2.6,  2.7,  2.8, 12.9,
     13.0, 13.1, 13.2, 13.3, 13.4, 13.5, 13.6, 13.7, 13.8,  3.9,
      4.0,  4.1,  4.2,  4.3,  4.4,  4.5,  4.6,  4.7,  4.8, 14.9]


switch_trs =
    [ switch_t0 ~= switch_t0r,
      switch_t1 ~= switch_t1r,
      switch_t2 ~= switch_t2r,
      switch_t3 ~= switch_t3r,
      switch_t4 ~= switch_t4r,
      switch_t5 ~= switch_t5r
    ]

switch_tr = and switch_trs

