-- |
-- Module      : TestSpaceAccum
-- Description : Test cases for accumulators
-- Copyright   : Yale University, 2003
--               University of Nottingham, 2005
-- Authors     : Antony Courtney and Henrik Nilsson
module TestSpaceAccum
    ( accum_st0
    , accum_st0r
    , accum_st1
    , accum_st1r
    )
  where

import FRP.Yampa

import TestsCommon

accum_st0 :: Double
accum_st0 = testSFSpaceLeak 1000000
                            (repeatedly 1.0 1.0
                             >>> accumBy (+) 0.0
                             >>> hold (-99.99))

accum_st0r = 249999.0

accum_st1 :: Double
accum_st1 = testSFSpaceLeak 1000000
                            (arr dup
                             >>> first (repeatedly 1.0 1.0)
                             >>> arr (\(e,a) -> tag e a)
                             >>> accumFilter accumFun 0.0
                             >>> hold (-99.99))
  where
    accumFun c a | even (floor a) = (c+a, Just (c+a))
                 | otherwise      = (c, Nothing)

accum_st1r = 6.249975e10
