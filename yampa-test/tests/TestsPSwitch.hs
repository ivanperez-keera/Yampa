-- |
-- Module      : TestsPSwitch
-- Description : Test cases for pSwitchB and dpSwitchB
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module TestsPSwitch
    ( pswitch_st0
    , pswitch_st0r
    , pswitch_st1
    , pswitch_st1r
    )
  where

import Data.List (findIndex)

import FRP.Yampa

import TestsCommon

-- Starts three "ramps" with different phase. As soon as one exceeds a
-- threshold, it's restarted, while the others are left alone. The
-- observaton of the output is done via the loop (rather than the directly
-- from the outputs of the signal functions in the collection), thus the
-- use of a delayed switch is essential.
pswitch_ramp :: Double -> SF a Double
pswitch_ramp phase = constant 2.0 >>> integral >>> arr (+phase)

-- We assume that only one signal function will reach the limit at a time.
pswitch_limit :: Double -> SF ((a, [Double]), b) (Event Int)
pswitch_limit x = arr (snd . fst) >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t4rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t4rec sfs n =
  dpSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
            (pswitch_limit 2.99)
            pswitch_t4rec

-- Variation of the test above, with direct observation (not via loop) and
-- immediate switch.
--
-- We assume that only one signal function will reach the limit at a time.
pswitch_limit2 :: Double -> SF (a, [Double]) (Event Int)
pswitch_limit2 x = arr snd >>> arr (findIndex (>=x)) >>> edgeJust

pswitch_t5rec :: [SF (a, [Double]) Double]
                 -> Int
                 -> SF (a, [Double]) [Double]
pswitch_t5rec sfs n =
  pSwitchB (take n sfs ++ [pswitch_ramp 0.0] ++ drop (n+1) sfs)
           (pswitch_limit2 2.99)
           pswitch_t5rec

pswitch_st0 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) ([Double],[Double])
    sf = dpSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                   (pswitch_limit 2.99)
                   pswitch_t4rec
         >>> arr dup

pswitch_st0r = [1.5,2.5,0.5]

pswitch_st1 = testSFSpaceLeak 1000000 (loop sf)
  where
    sf :: SF (a, [Double]) (([Double], Double), [Double])
    sf = ((pSwitchB [pswitch_ramp 0.0, pswitch_ramp 1.0, pswitch_ramp 2.0]
                    (pswitch_limit2 2.99)
                    pswitch_t5rec)
          &&& (arr snd >>> arr sum))
         >>> arr (\(xs, y) -> ((xs, y), xs))

pswitch_st1r = ([1.5,2.5,0.5],4.5)
