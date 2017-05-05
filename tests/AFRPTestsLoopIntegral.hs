{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsLoopIntegral.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsLoopIntegral				     *
*       Purpose:        Test cases for loopIntegral			     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsLoopIntegral (loopIntegral_tr, loopIntegral_trs) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases for loopIntegral
------------------------------------------------------------------------------

-- Computation of approximation to exp 0, exp 1, ..., exp 5 by integration.
-- Values as given by using exp directly:
-- 1.0, 2.71828, 7.38906, 20.0855, 54.5981, 148.413
loopIntegral_t0 =
    let
	es = embed (loopIntegral (arr (\(_, x) -> (x + 1, x + 1))))
                   (deltaEncode 0.001 (repeat ()))
    in
	[es!!0, es!!1000, es!!2000, es!!3000, es!!4000, es!!5000]
loopIntegral_t0r :: [Double]
loopIntegral_t0r = [1.0,2.71692,7.38167,20.05544,54.48911,148.04276]


-- Test case with a time varying signal transformer inside the loop.
-- Starting at position 0 [m], accelerate by 1.0 [m/s^2] until position
-- exceeds 2.0 [m]. Then accelerate by -1.0 [m/s^2] until position gets
-- below 0.0 [m]. Then accelerate at 1.0 [m/s^2] again. And so on.

type Position = Double
type Velocity = Double
type Acceleration = Double

posCntrl :: SF b Position
posCntrl = loopIntegral posCntrlNR
    where
	posCntrlNR :: SF (b, Velocity) (Position, Acceleration)
	posCntrlNR =
	    arr snd			-- Get the velocity.
	    >>> integral		-- This integral gives us the position.
	    >>> arr (\x -> (x,x))
	    >>>
		(second $
		    arr (\x -> (x,x))
		    >>>
			(first $
			    arr (>=2.0)
			    >>> edge
			    >>> (arr (fmap (const (constant (-1.0))))))
		    >>>
			(second $
			    arr (< 0.0)
			    >>> edge
			    >>> (arr (fmap (const (constant 1.0)))))
		    >>> arr (\(e1,e2) -> e1 `lMerge` e2)
		    >>> arr (\e -> ((), e))
		    >>> rSwitch (constant 1.0))


loopIntegral_t1 = take 250 (embed posCntrl (deltaEncode 0.1 (repeat ())))

-- Result only partially verified. But the sign of the acceleration changes
-- at roughly the right points.
loopIntegral_t1r :: [Double]
loopIntegral_t1r =
    [0.0,0.0,0.01,0.03,0.06,0.1,0.15,0.21,0.28,0.36,0.45,0.55,0.66,0.78,0.91,
     1.05,1.2,1.36,1.53,1.71,1.9,2.1,2.31,2.51,2.7,2.88,3.05,3.21,3.36,3.5,
     3.63,3.75,3.86,3.96,4.05,4.13,4.2,4.26,4.31,4.35,4.38,4.4,4.41,4.41,4.4,
     4.38,4.35,4.31,4.26,4.2,4.13,4.05,3.96,3.86,3.75,3.63,3.5,3.36,3.21,3.05,
     2.88,2.7,2.51,2.31,2.1,1.88,1.65,1.41,1.16,0.9,0.63,0.35,0.06,-0.24,
     -0.55,-0.85,-1.14,-1.42,-1.69,-1.95,-2.2,-2.44,-2.67,-2.89,-3.1,-3.3,
     -3.49,-3.67,-3.84,-4.0,-4.15,-4.29,-4.42,-4.54,-4.65,-4.75,-4.84,-4.92,
     -4.99,-5.05,-5.1,-5.14,-5.17,-5.19,-5.2,-5.2,-5.19,-5.17,-5.14,-5.1,
     -5.05,-4.99,-4.92,-4.84,-4.75,-4.65,-4.54,-4.42,-4.29,-4.15,-4.0,-3.84,
     -3.67,-3.49,-3.3,-3.1,-2.89,-2.67,-2.44,-2.2,-1.95,-1.69,-1.42,-1.14,
     -0.85,-0.55,-0.24,0.08,0.41,0.75,1.1,1.46,1.83,2.21,2.6,2.98,3.35,3.71,
     4.06,4.4,4.73,5.05,5.36,5.66,5.95,6.23,6.5,6.76,7.01,7.25,7.48,7.7,7.91,
     8.11,8.3,8.48,8.65,8.81,8.96,9.1,9.23,9.35,9.46,9.56,9.65,9.73,9.8,9.86,
     9.91,9.95,9.98,10.0,10.01,10.01,10.0,9.98,9.95,9.91,9.86,9.8,9.73,9.65,
     9.56,9.46,9.35,9.23,9.1,8.96,8.81,8.65,8.48,8.3,8.11,7.91,7.7,7.48,7.25,
     7.01,6.76,6.5,6.23,5.95,5.66,5.36,5.05,4.73,4.4,4.06,3.71,3.35,2.98,2.6,
     2.21,1.81,1.4,0.98,0.55,0.11,-0.34,-0.80,-1.25,-1.69,-2.12,-2.54,-2.95,
     -3.35,-3.74,-4.12,-4.49,-4.85,-5.2,-5.54,-5.87,-6.19,-6.5,-6.8,-7.09,
     -7.37,-7.64,-7.9]


loopIntegral_trs =
    [ loopIntegral_t0 ~= loopIntegral_t0r,
      loopIntegral_t1 ~= loopIntegral_t1r
    ]

loopIntegral_tr = and loopIntegral_trs
