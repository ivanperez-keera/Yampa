{-# OPTIONS_GHC -fno-warn-tabs #-}
{- $Id: AFRPTestsLaws.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                                  A F R P                                   *
*                                                                            *
*       Module:         AFRPTestsLaws                                        *
*       Purpose:        Test cases based on the arrow laws		     *
*	Authors:	Antony Courtney and Henrik Nilsson		     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module AFRPTestsLaws (laws_trs, laws_tr) where

import FRP.Yampa

import AFRPTestsCommon

------------------------------------------------------------------------------
-- Test cases based on the arrow laws
------------------------------------------------------------------------------

-- For a description of the laws, see e.g. Ross Paterson: Embedding a Class of
-- Domain-Specific Languages in a Functional Language.
-- Only a very rudimentary sanity check. Obviously not intended to "prove"
-- this implementation indeed do respect the laws.

laws_t0_lhs :: [Double]
laws_t0_lhs = testSF1 (arr id >>> integral)
laws_t0_rhs :: [Double]
laws_t0_rhs = testSF1 (integral)

laws_t1_lhs :: [Double]
laws_t1_lhs = testSF1 (integral >>> arr id)
laws_t1_rhs :: [Double]
laws_t1_rhs = testSF1 (integral)

laws_t2_lhs :: [Double]
laws_t2_lhs = testSF1 ((integral >>> arr (*0.5)) >>> integral)
laws_t2_rhs :: [Double]
laws_t2_rhs = testSF1 (integral >>> (arr (*0.5) >>> integral))

laws_t3_lhs :: [Double]
laws_t3_lhs = testSF1 (arr ((*2.5) . (+3.0)))
laws_t3_rhs :: [Double]
laws_t3_rhs = testSF1 (arr (+3.0) >>> arr (*2.5))

laws_t4_lhs :: [(Double, Double)]
laws_t4_lhs = testSF1 (arr dup >>> first (arr (*2.5)))
laws_t4_rhs :: [(Double, Double)]
laws_t4_rhs = testSF1 (arr dup >>> arr ((*2.5) *** id))

laws_t5_lhs :: [(Double, Double)]
laws_t5_lhs = testSF1 (arr dup >>> (first (integral >>> arr (+3.0))))
laws_t5_rhs :: [(Double, Double)]
laws_t5_rhs = testSF1 (arr dup >>> (first integral >>> first (arr (+3.0))))

laws_t6_lhs :: [(Double, Double)]
laws_t6_lhs = testSF1 (arr dup >>> (first integral >>> arr (id *** (+3.0))))
laws_t6_rhs :: [(Double, Double)]
laws_t6_rhs = testSF1 (arr dup >>> (arr (id *** (+3.0)) >>> first integral))

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


laws_trs =
    [ laws_t0_lhs ~= laws_t0_rhs,
      laws_t1_lhs ~= laws_t1_rhs,
      laws_t2_lhs ~= laws_t2_rhs,
      laws_t3_lhs ~= laws_t3_rhs,
      laws_t4_lhs ~= laws_t4_rhs,
      laws_t5_lhs ~= laws_t5_rhs,
      laws_t6_lhs ~= laws_t6_rhs,
      laws_t7_lhs ~= laws_t7_rhs,
      laws_t8_lhs ~= laws_t8_rhs
    ]

laws_tr = and laws_trs
