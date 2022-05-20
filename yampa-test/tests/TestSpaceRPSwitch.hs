-- |
-- Module      : TestsRPSwitch
-- Description : Test cases for rpSwitchB and drpSwitchB
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module TestsRPSwitch
    ( rpswitch_st0
    , rpswitch_st0r
    )
  where

import Data.Maybe (fromJust)
import Data.List (findIndex)

import FRP.Yampa

import TestsCommon

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The observaton
-- of the output is done via a loop, thus the  use of a delayed switch is
-- essential.

rpswitch_ramp :: Double -> SF a Double
rpswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
rpswitch_limit :: Double -> SF [Double] (Event ([SF a Double]->[SF a Double]))
rpswitch_limit x = arr (findIndex (>=x)) >>> edgeJust >>> arr (fmap restart)
  where
    restart n = \sfs -> take n sfs ++ [rpswitch_ramp 0.0] ++ drop (n+1) sfs

rpswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = (second (rpswitch_limit 2.99)
          >>> drpSwitchB [ rpswitch_ramp 0.0
                         , rpswitch_ramp 1.0
                         , rpswitch_ramp 2.0
                         ]
         ) >>> arr dup

rpswitch_st0r = [1.5,2.5,0.5]
