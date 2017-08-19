-- |
-- Module      : FRP.Yampa.Time
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- SF primitives that producing the current running time.
--
-- Time is global for an 'SF', so, every constituent 'SF' will use the same
-- global clock. However, when used in combination with
-- 'FRP.Yampa.Switches.switch'ing, the SF switched into will be started at the
-- time of switching, so any reference to 'localTime' or 'time' from that 'SF'
-- will count using the time of switching as the start time.
--
-- Take also into account that, because 'FRP.Yampa.Integration.derivative' is
-- the derivative of a signal /over time/, differentiating 'localTime' will
-- always produce the value one (@1@). If you really, really, really need to
-- know the time delta, and need to abandon the hybrid\/FRP abstraction, see
-- 'FRP.Yampa.Integration.iterFrom'.
module FRP.Yampa.Time
    ( localTime
    , time
    , timeTransform
    , timeTransformSF
    )
  where

-- External imports
import Control.Arrow ((>>>))

-- Internal imports
import FRP.Yampa.Basic        (constant)
import FRP.Yampa.Integration  (integral)
import FRP.Yampa.InternalCore (DTime, SF (SF), SF' (SF'), Time, sfTF, sfTF')

-- | Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: SF a Time
time = localTime

-- ** Time transformations (run SFs slower/faster)

-- NOTE: These versions are not optimized.

timeTransform :: (DTime -> DTime) -> SF a b -> SF a b
timeTransform transform sf = SF tf
 where tf a = let (sf', b) = (sfTF sf) a
                  sf''     = timeTransformF transform sf'
              in (sf'', b)

timeTransformF :: (DTime -> DTime) -> SF a b -> SF a b
timeTransformF transform sf = SF' tf
 where tf dt a = let dt'      = transform dt
                     (sf', b) = (sfTF' sf) dt' a
                     sf''     = timeTransformF transform sf'
                 in if dt' <= 0
                          then usrErr "AFRP" "timeTransform" "The time cannot be negative"
                          else (sf'', b)

timeTransformSF :: SF a (DTime -> DTime)
                -> SF a b
                -> SF a b
timeTransformSF transformSF sf = SF tf
 where tf a = let (transformSFF, _) = (sfTF transformSF) a
                  (sf', b) = (sfTF sf) a
                  sf''     = timeTransformSFF transformSFF sf'
              in (sf'', b)

timeTransformSFF :: SF' a (DTime -> DTime) -> SF' a b -> SF' a b
timeTransformSFF transformSF sf = SF' tf
 where tf dt a = let (transformSF', transform) = (sfTF' transformSF) dt a
                     dt'                           = transform dt
                     (sf', b)                      = (sfTF' sf) dt' a
                     sf''                          = timeTransformSFF transformSF' sf'
                 in (sf'', b)
