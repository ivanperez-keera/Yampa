-- |
-- Module      : TestSpaceRSwitch
-- Description : Test cases for rSwitch and drSwitch
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module TestSpaceRSwitch
    ( rswitch_st0
    , rswitch_st0r
    )
  where

import Data.Maybe (fromJust)

import FRP.Yampa

import TestsCommon

rswitch_sawTooth :: SF a Double
rswitch_sawTooth =
    loop (second (arr (>=5.0)
                  >>> edge
                  >>> arr (`tag` ramp))
          >>> drSwitch ramp
          >>> arr dup)
  where
    ramp :: SF a Double
    ramp = constant 1.0 >>> integral

rswitch_st0 = testSFSpaceLeak 2000000 rswitch_sawTooth
rswitch_st0r = 4.75
