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
) where

import Control.Arrow

import FRP.Yampa.InternalCore (SF, Time)
import FRP.Yampa.Basic (constant)
import FRP.Yampa.Integration (integral)

-- | Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: SF a Time
time = localTime

-- Vim modeline
-- vim:set tabstop=8 expandtab:
