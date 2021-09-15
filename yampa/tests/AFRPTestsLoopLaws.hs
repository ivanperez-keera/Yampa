{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsLoopLaws.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsLoopLaws                                    *
*       Purpose:        Test cases based on the arrow laws for loop	     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsLoopLaws (looplaws_trs, looplaws_tr) where

import Data.Tuple(swap)

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases based on the arrow laws for loop
------------------------------------------------------------------------------

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


looplaws_trs =
    [ looplaws_t0_lhs  ~= looplaws_t0_rhs,
      looplaws_t1_lhs  ~= looplaws_t1_rhs,
      looplaws_t2_lhs  ~= looplaws_t2_rhs,
      looplaws_t3_lhs  ~= looplaws_t3_rhs,
      looplaws_t4_lhs  ~= looplaws_t4_rhs,
      looplaws_t5_lhs  ~= looplaws_t5_rhs
    ]

looplaws_tr = and looplaws_trs
