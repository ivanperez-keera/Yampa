{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Loop
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
--
-- Portability :  non-portable -GHC extensions-
--
-- Well-initialised loops
-----------------------------------------------------------------------------------------

module FRP.Yampa.Loop (
    -- * Loops with guaranteed well-defined feedback
    loopPre,            -- :: c -> SF (a,c) (b,c) -> SF a b
    loopIntegral,       -- :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
) where


import Control.Arrow

import FRP.Yampa.InternalCore (SF)

import FRP.Yampa.Integration
import FRP.Yampa.Delays
import FRP.Yampa.VectorSpace

-- * Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: c -> SF (a,c) (b,c) -> SF a b
loopPre c_init sf = loop (second (iPre c_init) >>> sf)

-- | Loop by integrating the second value in the pair and feeding the
-- result back. Because the integral at time 0 is zero, this is always
-- well defined.
loopIntegral :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)

-- Vim modeline
-- vim:set tabstop=8 expandtab:
