{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsDelay.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsDelay					     *
*       Purpose:        Test cases for delays				     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsDelay (delay_tr, delay_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for delays
------------------------------------------------------------------------------

delay_t0 = testSF1 (delay 0.0 undefined)
delay_t0r =
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,
     15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0]

delay_t1 = testSF1 (delay 0.0001 17)
delay_t1r =
    [17.0,0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,
     15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0]

delay_t2 = testSF2 (delay 0.0001 17)
delay_t2r =
    [17.0,0.0,0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,2.0,
     3.0,3.0,3.0,3.0,3.0,4.0,4.0,4.0,4.0]

delay_t3 = testSF1 (time 
                    >>> arr (\t -> sin (0.5 * t * pi + pi))
                    >>> loop (arr (\(x1,x2) -> let x' = max x1 x2 in (x',x')) 
                              >>> second (delay 0.0001 0.0)))
delay_t3r = 
    take 25
         (let xs = [ sin (0.5 * t * pi + pi) | t <- [0.0, 0.25 ..] ]
          in tail (scanl max 0 xs))

dts_t4 = take 15 (repeat 0.1)
         ++ [0.5, 0.5]
         ++ take 15 (repeat 0.1)
         ++ [2.0]
         ++ take 20 (repeat 0.1)

input_t4 = (0, [ (dt, Just i) | (dt, i) <- zip dts_t4 [1..] ])

delay_t4, delay_t4r :: [Int]
delay_t4 = take 100 (embed (delay 1.05 (-1)) input_t4)
delay_t4r =
    [ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,	-- 0.0 s -- 0.9 s
      -1,  0,  1,  2,  3,  4,			-- 1.0 s -- 1.5 s
       9,                 14, 15, 15, 15, 15,	-- 2.0 s -- 2.9 s
      15, 16, 16, 16, 16, 16, 17, 18, 19, 20,	-- 3.0 s -- 3.9 s
      21,					-- 4.0 s
      32, 32, 32, 32, 32, 32, 32, 32, 32, 32,	-- 6.0 s -- 6.9 s
      32, 33, 34, 35, 36, 37, 38, 39, 40, 41,	-- 7.0 s -- 7.9 s
      42					-- 8.0 s
    ]


delay_t5 = take 100 (drop 6 (embed sf (deltaEncode 0.1 (repeat ()))))
    where
        sf = time >>> arr (\t -> sin (2*pi*t)) >>> delay 0.55 (-1.0)

delay_t5r = take 100 (drop 6 (embed sf (deltaEncode 0.1 (repeat ()))))
    where
        sf = time >>> arr (\t -> sin (2*pi*(t-0.6)))


delay_trs =
    [ delay_t0 ~= delay_t0r,
      delay_t1 ~= delay_t1r,
      delay_t2 ~= delay_t2r,
      delay_t3 ~= delay_t3r,
      delay_t4 == delay_t4r,
      delay_t5 ~= delay_t5r
    ]

delay_tr = and delay_trs
