{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id$
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsSscan					     *
*       Purpose:        Test cases for pre sscan	     		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*             Copyright (c) University of Nottingham, 2005                   *
*                                                                            *
******************************************************************************
-}

module AFRPTestsSscan (sscan_tr, sscan_trs) where

import FRP.Yampa
import FRP.Yampa.Internals

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases sscan
------------------------------------------------------------------------------

-- pre and iPre in terms of sscan
pre_sscan :: SF a a
pre_sscan = sscanPrim f uninit uninit
    where
        f c a = Just (a, c)
        uninit = error "pre_sscan: Uninitialized pre operator."

iPre_sscan :: a -> SF a a
iPre_sscan = (--> pre_sscan)


sscan_t0, sscan_t0r :: [Double]
sscan_t0 = testSF1 (iPre_sscan 17)
sscan_t0r =
    [17.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,
     15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0]


sscan_t1, sscan_t1r :: [Double]
sscan_t1 = testSF2 (iPre_sscan 17)
sscan_t1r =
    [17.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,
     3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0]


sscan_t2, sscan_t2r :: [Double]
sscan_t2 = testSF1 (time 
                    >>> arr (\t -> sin (0.5 * t * pi + pi))
                    >>> loop (arr (\(x1,x2) -> let x' = max x1 x2 in (x',x')) 
                              >>> second (iPre_sscan 0.0)))
sscan_t2r = 
    take 25
         (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
          in tail (scanl max 0 xs))



sscan_t3, sscan_t3r :: [Double]
sscan_t3 = testSF1 (time 
                    >>> arr (\t -> sin (0.5 * t * pi + pi))
                    >>> sscan max 0.0)

sscan_t3r = 
    take 25
         (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
          in tail (scanl max 0 xs))


hold_sscan :: a -> SF (Event a) a
hold_sscan a = sscanPrim f () a
    where
        f _ NoEvent   = Nothing 
        f _ (Event a) = Just ((), a)


dHold_sscan :: a -> SF (Event a) a
dHold_sscan a = hold_sscan a >>> iPre_sscan a


-- This is a (somewhat strange) way of doing a counter that
-- stops after reaching a threshold. Note that the ingoing event
-- is *control dependent* on the output of the counter, so
-- "dHold" really has to have the capability of delivering an
-- output without looking at the current input at all.
sscan_t4, sscan_t4r :: [Int]
sscan_t4 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = repeatedly 1.0 ()
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                      >>> dHold_sscan 0
                      >>> arr dup)
sscan_t4r = [0,0,0,0,		-- 0s
             0,1,1,1,		-- 1s
             1,2,2,2,		-- 2s
             2,3,3,3,		-- 3s
             3,4,4,4,		-- 4s
             4,5,5,5,		-- 5s
             5,6,6,6,		-- 6s
             6,7,7,7,		-- 7s
             7,8,8,8,		-- 8s
             8,9,9,9,		-- 9s
             9,10,10,10,	-- 10s
             10,10,10,10,	-- 11s
             10,10]		-- 12s

-- Version of the above that tests that thigs still work OK also if
-- there is an initial event.
sscan_t5, sscan_t5r :: [Int]
sscan_t5 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                      >>> dHold_sscan 0
                      >>> arr dup)
sscan_t5r = [0,1,1,1,		-- 0s 
             1,2,2,2,		-- 1s 
             2,3,3,3,		-- 2s 
             3,4,4,4,		-- 3s 
             4,5,5,5,		-- 4s 
             5,6,6,6,		-- 5s 
             6,7,7,7,		-- 6s 
             7,8,8,8,		-- 7s 
             8,9,9,9,		-- 8s 
             9,10,10,10,	-- 9s 
             10,10,10,10,	-- 10s
             10,10,10,10,	-- 11s
             10,10]		-- 12s


-- Version of the sscan_t4 in terms of sscan
sscan_t6, sscan_t6r :: [Int]
sscan_t6 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = repeatedly 1.0 () >>> (sscanPrim f 0 0)

        f c NoEvent               = Nothing
        f c (Event _) | c < 10    = Just (c', c')
                      | otherwise = Nothing
            where
	        c' = c + 1


sscan_t6r = [0,0,0,0,		-- 0s
             1,1,1,1,		-- 1s
             2,2,2,2,		-- 2s
             3,3,3,3,		-- 3s
             4,4,4,4,		-- 4s
             5,5,5,5,		-- 5s
             6,6,6,6,		-- 6s
             7,7,7,7,		-- 7s
             8,8,8,8,		-- 8s
             9,9,9,9,		-- 9s
             10,10,10,10,	-- 10s
             10,10,10,10,	-- 11s
             10,10]		-- 12s

-- Version of sscan_t5 directly in terms of sscan.
sscan_t7, sscan_t7r :: [Int]
sscan_t7 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
             >>> (sscanPrim f 0 0)

        f c NoEvent               = Nothing
        f c (Event _) | c < 10    = Just (c', c')
                      | otherwise = Nothing
            where
	        c' = c + 1
        

sscan_t7r = [1,1,1,1,		-- 0s 
             2,2,2,2,		-- 1s 
             3,3,3,3,		-- 2s 
             4,4,4,4,		-- 3s 
             5,5,5,5,		-- 4s 
             6,6,6,6,		-- 5s 
             7,7,7,7,		-- 6s 
             8,8,8,8,		-- 7s 
             9,9,9,9,		-- 8s 
             10,10,10,10,	-- 9s 
             10,10,10,10,	-- 10s
             10,10,10,10,	-- 11s
             10,10]		-- 12s


edge_sscan :: SF Bool (Event ())
edge_sscan = sscanPrim f 2 NoEvent
    where
        f 0 False = Nothing
        f 0 True  = Just (1, Event ())
        f 1 False = Just (0, NoEvent)
        f 1 True  = Just (2, NoEvent)
        f 2 False = Just (0, NoEvent)
        f 2 True  = Nothing


sscan_t8 :: [Event ()]
sscan_t8 = testSF1 (localTime >>> arr (>=0) >>> edge_sscan)

sscan_t8r = 
    [NoEvent, NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 3.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	NoEvent,	-- 5.0 s
     NoEvent]


sscan_t9 :: [Event ()]
sscan_t9 = testSF1 (localTime >>> arr (>=4.26) >>> edge_sscan)

sscan_t9r =
    [NoEvent, NoEvent, NoEvent,  NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent,  NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 3.0 s
     NoEvent, NoEvent, Event (), NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 5.0 s
     NoEvent]


edgeBy_sscan :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy_sscan f a = sscanPrim g a NoEvent
    where
        g a_prev a = Just (a, maybeToEvent (f a_prev a))


-- Raising edge detector.
sscan_isEdge False False = Nothing
sscan_isEdge False True  = Just ()
sscan_isEdge True  True  = Nothing
sscan_isEdge True  False = Nothing


sscan_t10 :: [Event ()]
sscan_t10 = testSF1 (localTime
                     >>> arr (>=0)
                     >>> edgeBy_sscan sscan_isEdge False)

sscan_t10r = 
    [Event (), NoEvent, NoEvent, NoEvent,	-- 0.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 1.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 2.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 3.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 4.0 s
     NoEvent,  NoEvent, NoEvent, NoEvent,	-- 5.0 s
     NoEvent]

sscan_t11 :: [Event ()]
sscan_t11 = testSF1 (localTime 
                     >>> arr (>=4.26)
                     >>> edgeBy_sscan sscan_isEdge False)

sscan_t11r =
    [NoEvent, NoEvent, NoEvent,  NoEvent,	-- 0.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 1.0 s
     NoEvent, NoEvent, NoEvent,  NoEvent,	-- 2.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 3.0 s
     NoEvent, NoEvent, Event (), NoEvent,	-- 4.0 s
     NoEvent, NoEvent, NoEvent,	 NoEvent,	-- 5.0 s
     NoEvent]

-- Raising and falling edge detector.
sscan_isEdge2 False False = Nothing
sscan_isEdge2 False True  = Just True
sscan_isEdge2 True  True  = Nothing
sscan_isEdge2 True  False = Just False

sscan_t12 :: [Event Bool]
sscan_t12 = testSF1 (localTime
                    >>> arr (\t -> t >=2.01 && t <= 4.51)
		    >>> edgeBy_sscan sscan_isEdge2 True)

sscan_t12r =
    [Event False, NoEvent,    NoEvent, NoEvent,		-- 0.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 1.0 s
     NoEvent,     Event True, NoEvent, NoEvent,		-- 2.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 3.0 s
     NoEvent,     NoEvent,    NoEvent, Event False,	-- 4.0 s
     NoEvent,     NoEvent,    NoEvent, NoEvent,		-- 5.0 s
     NoEvent]



smaximum_sscan :: Ord a => SF a a
smaximum_sscan =
    switch (identity &&& now () >>> arr (\(a,e) -> (a, e `tag` a)))
           (\a0 -> sscanPrim (\c a -> if a > c
                                      then (Just (a,a))
                                      else Nothing)
                             a0 a0)


sscan_t13, sscan_t13r :: [Double]
sscan_t13 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf = time
             >>> arr (\t -> (t + 1) * cos (pi * t + pi))
             >>> smaximum_sscan

sscan_t13r =
    take 100
         (let xs = [ (t + 1) * cos (pi * t + pi) | t <- [0.0, 0.1 ..] ]
          in tail (scanl max (-100) xs))


-- Some tests of signal functions that may be implemented using sscan
-- internally and their combinations with other sscan-based signal
-- functions and event processors.

sscan_t14, sscan_t14r :: [Event Int]
sscan_t14 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf :: SF () (Event Int)
        sf = time >>> arr (\t -> sin (2 * t))
             >>> arr (>0)
             >>> edge
             >>> arr (`tag` (+1))
             >>> accum 0

sscan_t14r =
    [NoEvent,Event 1,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,Event 2,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,Event 3,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     Event 4,NoEvent,NoEvent,NoEvent,NoEvent]

sscan_t15, sscan_t15r :: [Int]
sscan_t15 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf :: SF () Int
        sf = time >>> arr (\t -> sin (2 * t))
             >>> arr (>0)
             >>> edge
             >>> arr (`tag` (+1))
             >>> accumHold 0

sscan_t15r =
    [0,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,2,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,3,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,4,4,4,4,4]

sscan_t16, sscan_t16r :: [Int]
sscan_t16 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf :: SF () Int
        sf = time >>> arr (\t -> sin (2 * t))
             >>> arr (>0)
             >>> edge
             >>> arr (`tag` (+1))
             >>> dAccumHold 0

sscan_t16r =
    [0,0,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,1,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,2,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,3,4,4,4,4]

sscan_t17, sscan_t17r :: [Event Int]
sscan_t17 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf :: SF () (Event Int)
        sf = time >>> arr (\t -> sin (2 * t))
             >>> arr (>0)
             >>> iPre False
             >>> edge
             >>> arr (`tag` (+1))
             >>> accum 0

sscan_t17r =
    [NoEvent,NoEvent,Event 1,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,Event 2,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,Event 3,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,NoEvent,NoEvent,NoEvent,NoEvent,
     NoEvent,Event 4,NoEvent,NoEvent,NoEvent]

sscan_t18, sscan_t18r :: [Int]
sscan_t18 = take 100 (embed sf (deltaEncode 0.1 (repeat ())))
    where
        sf :: SF () Int
        sf = time >>> arr (\t -> sin (2 * t))
             >>> arr (>0)
             >>> iPre False
             >>> edge
             >>> arr (`tag` (+1))
             >>> accumHold 0

sscan_t18r = 
    [0,0,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,1,1,1,1,1,1,1,1,
     1,1,1,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,2,2,2,2,2,2,2,
     2,2,2,2,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,3,3,3,3,3,
     3,3,3,3,3,3,4,4,4,4]

sscan_trs =
    [ sscan_t0 ~= sscan_t0r,
      sscan_t1 ~= sscan_t1r,
      sscan_t2 ~= sscan_t2r,
      sscan_t3 ~= sscan_t3r,
      sscan_t4 == sscan_t4r,
      sscan_t5 == sscan_t5r,
      sscan_t6 == sscan_t6r,
      sscan_t7 == sscan_t7r,
      sscan_t8 == sscan_t8r,
      sscan_t9 == sscan_t9r,
      sscan_t10 == sscan_t10r,
      sscan_t11 == sscan_t11r,
      sscan_t12 == sscan_t12r,
      sscan_t13 ~= sscan_t13r,
      sscan_t14 ~= sscan_t14r,
      sscan_t15 ~= sscan_t15r,
      sscan_t16 ~= sscan_t16r,
      sscan_t17 ~= sscan_t17r,
      sscan_t18 ~= sscan_t18r
    ]

sscan_tr = and sscan_trs
