{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Time
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Time (
    localTime,          -- :: SF a Time
    time,               -- :: SF a Time,        Other name for localTime.
    timeTransform,      -- :: (DTime -> DTime) -> SF a b -> SF a b
) where

import Control.Arrow

import FRP.Yampa.Basic        (constant)
import FRP.Yampa.Diagnostics  (usrErr)
import FRP.Yampa.Integration  (integral)
import FRP.Yampa.InternalCore ( SF(SF), SF'(SF')
                              , sfTF,   sfTF'
                              , Time,   DTime
                              )

-- | Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: SF a Time
time = localTime

-- ** Time transformations (run SFs slower/faster)

-- NOTE: These versions are not optimized.
type Endo a = a -> a

timeTransform :: Endo DTime -> SF a b -> SF a b
timeTransform transform sf = SF tf
 where tf a = let (sf', b) = (sfTF sf) a
                  sf''     = timeTransformF transform sf'
              in (sf'', b)

timeTransformF :: Endo DTime -> SF' a b -> SF' a b
timeTransformF transform sf = SF' tf
 where tf dt a = let dt'      = transform dt
                     (sf', b) = (sfTF' sf) dt' a
                     sf''     = timeTransformF transform sf'
                 in if dt' <= 0
                          then usrErr "AFRP" "timeTransform" "The time cannot be negative"
                          else (sf'', b)

-- Vim modeline
-- vim:set tabstop=8 expandtab:
