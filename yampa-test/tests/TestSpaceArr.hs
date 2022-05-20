-- |
-- Module      : TestSpaceArr
-- Description : Test cases for arr
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module TestSpaceArr
    ( arr_st0
    , arr_st0r
    , arr_st1
    , arr_st1r
    )
  where

import FRP.Yampa

import TestsCommon

arr_st0 = testSFSpaceLeak 2000000 (arr (+1))
arr_st0r = 1000000.5

arr_st1 = testSFSpaceLeak 2000000 identity
arr_st1r = 999999.5
