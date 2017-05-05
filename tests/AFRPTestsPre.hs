{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsDelay.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsPre					     *
*       Purpose:        Test cases for pre and (derived) combinators	     *
*			that (semantically) involves a pre.		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*             Copyright (c) University of Nottingham, 2005                   *
*                                                                            *
******************************************************************************
-}

module AFRPTestsPre (pre_tr, pre_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for pre and related combinators
------------------------------------------------------------------------------

pre_t0 = testSF1 (iPre 17)
pre_t0r =
    [17.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,
     15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0]

pre_t1 = testSF2 (iPre 17)
pre_t1r =
    [17.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,
     3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0]

pre_t2 = testSF1 (time 
                  >>> arr (\t -> sin (0.5 * t * pi + pi))
                  >>> loop (arr (\(x1,x2) -> let x' = max x1 x2 in (x',x')) 
                            >>> second (iPre 0.0)))

pre_t2r = 
    take 25
         (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
          in tail (scanl max 0 xs))


-- This is a (somewhat strange) way of doing a counter that
-- stops after reaching a threshold. Note that the ingoing event
-- is *control dependent* on the output of the counter, so
-- "dHold" really has to have the capability of delivering an
-- output without looking at the current input at all.
pre_t3, pre_t3r :: [Int]
pre_t3 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = repeatedly 1.0 ()
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                      >>> dHold 0
                      >>> arr dup)
pre_t3r = [0,0,0,0,	-- 0s
           0,1,1,1,	-- 1s
           1,2,2,2,	-- 2s
           2,3,3,3,	-- 3s
           3,4,4,4,	-- 4s
           4,5,5,5,	-- 5s
           5,6,6,6,	-- 6s
           6,7,7,7,	-- 7s
           7,8,8,8,	-- 8s
           8,9,9,9,	-- 9s
           9,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s

-- Version of the above that tests that thigs still work OK also if
-- there is an initial event.
pre_t4, pre_t4r :: [Int]
pre_t4 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (c + 1)) `gate` (c < 10))
                      >>> dHold 0
                      >>> arr dup)
pre_t4r = [0,1,1,1,	-- 0s 
           1,2,2,2,	-- 1s 
           2,3,3,3,	-- 2s 
           3,4,4,4,	-- 3s 
           4,5,5,5,	-- 4s 
           5,6,6,6,	-- 5s 
           6,7,7,7,	-- 6s 
           7,8,8,8,	-- 7s 
           8,9,9,9,	-- 8s 
           9,10,10,10,	-- 9s 
           10,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s


-- Similar test to "pre_t3" above but for dAccumHold.
pre_t5, pre_t5r :: [Int]
pre_t5 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = repeatedly 1.0 ()
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (+1)) `gate` (c < 10))
                      >>> dAccumHold 0
                      >>> arr dup)
pre_t5r = [0,0,0,0,	-- 0s
           0,1,1,1,	-- 1s
           1,2,2,2,	-- 2s
           2,3,3,3,	-- 3s
           3,4,4,4,	-- 4s
           4,5,5,5,	-- 5s
           5,6,6,6,	-- 6s
           6,7,7,7,	-- 7s
           7,8,8,8,	-- 8s
           8,9,9,9,	-- 9s
           9,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s


-- Similar test to "pre_t4" above but for dAccumHold.
pre_t6, pre_t6r :: [Int]
pre_t6 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
             >>> (loop $
                      arr (\(e,c) -> (e `tag` (+1)) `gate` (c < 10))
                      >>> dAccumHold 0
                      >>> arr dup)
pre_t6r = [0,1,1,1,	-- 0s 
           1,2,2,2,	-- 1s 
           2,3,3,3,	-- 2s 
           3,4,4,4,	-- 3s 
           4,5,5,5,	-- 4s 
           5,6,6,6,	-- 5s 
           6,7,7,7,	-- 6s 
           7,8,8,8,	-- 7s 
           8,9,9,9,	-- 8s 
           9,10,10,10,	-- 9s 
           10,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s


-- Similar test to "pre_t3" above but for dAccumHoldBy.
pre_t7, pre_t7r :: [Int]
pre_t7 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = repeatedly 1.0 ()
             >>> (loop $
                      arr (\(e,c) -> e `gate` (c < 10))
                      >>> dAccumHoldBy (\c _ -> c + 1) 0
                      >>> arr dup)
pre_t7r = [0,0,0,0,	-- 0s
           0,1,1,1,	-- 1s
           1,2,2,2,	-- 2s
           2,3,3,3,	-- 3s
           3,4,4,4,	-- 4s
           4,5,5,5,	-- 5s
           5,6,6,6,	-- 6s
           6,7,7,7,	-- 7s
           7,8,8,8,	-- 8s
           8,9,9,9,	-- 9s
           9,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s


-- Similar test to "pre_t4" above but for dAccumHoldBy.
pre_t8, pre_t8r :: [Int]
pre_t8 = take 50 (embed sf (deltaEncode 0.25 (repeat ())))
    where
        sf = (now () &&& repeatedly 1.0 ()) >>> arr (uncurry lMerge)
             >>> (loop $
                      arr (\(e,c) -> e `gate` (c < 10))
                      >>> dAccumHoldBy (\c _ -> c + 1) 0
                      >>> arr dup)
pre_t8r = [0,1,1,1,	-- 0s 
           1,2,2,2,	-- 1s 
           2,3,3,3,	-- 2s 
           3,4,4,4,	-- 3s 
           4,5,5,5,	-- 4s 
           5,6,6,6,	-- 5s 
           6,7,7,7,	-- 6s 
           7,8,8,8,	-- 7s 
           8,9,9,9,	-- 8s 
           9,10,10,10,	-- 9s 
           10,10,10,10,	-- 10s
           10,10,10,10,	-- 11s
           10,10]	-- 12s



pre_trs =
    [ pre_t0 ~= pre_t0r,
      pre_t1 ~= pre_t1r,
      pre_t2 ~= pre_t2r,
      pre_t3 == pre_t3r,
      pre_t4 == pre_t4r,
      pre_t5 == pre_t5r,
      pre_t6 == pre_t6r,
      pre_t7 == pre_t7r,
      pre_t8 == pre_t8r
    ]

pre_tr = and pre_trs
