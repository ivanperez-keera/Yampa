-- |
-- Module      :  FRP.Yampa.Time
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- SF primitives that producing the current running time.
--
-- Time is global for an 'SF', so, every constituent 'SF' will use the
-- same global clock. However, when used in combination with
-- 'FRP.Yampa.Switches.switch'ing, the SF switched into will be started at the
-- time of switching, so any reference to 'localTime' or 'time' from that 'SF'
-- will count using the time of switching as the start time.
--
-- Take also into account that, because 'FRP.Yampa.Integration.derivative' is
-- the derivative of a signal /over time/, derivating 'localTime' will always
-- produce the value one (@1@). If you really, really, really need to know the
-- time delta, and need to abandon the hybrid\/FRP abstraction, see
-- 'FRP.Yampa.Integration.iterFrom'.
module FRP.Yampa.Time (
    localTime,
    time,
) where

import Control.Arrow

import FRP.Yampa.Basic        (constant)
import FRP.Yampa.Integration  (integral)
import FRP.Yampa.InternalCore (SF, Time)

-- | Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: SF a Time
time = localTime
