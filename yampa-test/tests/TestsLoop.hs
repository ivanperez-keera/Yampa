-- |
-- Module      : TestsLoop
-- Description : Test cases for loop
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module TestsLoop
    ( loop_st0
    , loop_st0r
    , loop_st1
    , loop_st1r
    )
  where

import FRP.Yampa

import TestsCommon

-- * Test cases for loop

loop_acc :: SF (Double, Double) (Double, Double)
loop_acc = arr (\(x, y)->(x+y, x+y))

loop_st0 = testSFSpaceLeak 2000000
                           (loop (second (iPre 0) >>> loop_acc))
loop_st0r = 9.999995e11

-- A simple loop test taken from MiniYampa:
-- This results in pulling on the fed-back output during evaluation, because
-- switch is strict in its input sample:
loop_st1 :: Double
loop_st1 = testSFSpaceLeak 2000000
             (loop $ second $ (switch identity (const (arr fst))) >>> arr (\x -> (x + x + x + x + x + x + x,noEvent)) >>> (iPre (25, noEvent)))
loop_st1r = 999999.5
