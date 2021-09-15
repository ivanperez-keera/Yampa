{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-- Module      :  FRP.Yampa.Scan
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)

-- | Simple, stateful signal processing.
--
-- Scanning implements elementary, step-based accumulating over signal
-- functions by means of an auxiliary function applied to each input and to an
-- accumulator. For comparison with other FRP libraries and with stream
-- processing abstractions, think of fold.

module FRP.Yampa.Scan (
    sscan,              -- :: (b -> a -> b) -> b -> SF a b
    sscanPrim,          -- :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
) where

import FRP.Yampa.InternalCore (SF(..), sfSScan)

-- ** Simple, stateful signal processing

-- | Applies a function point-wise, using the last output as next input. This
-- creates a well-formed loop based on a pure, auxiliary function.

-- New sscan primitive. It should be possible to define lots of functions
-- in terms of this one. Eventually a new constructor will be introduced if
-- this works out.
sscan :: (b -> a -> b) -> b -> SF a b
sscan f b_init = sscanPrim f' b_init b_init
    where
        f' b a = let b' = f b a in Just (b', b')

-- | Generic version of 'sscan', in which the auxiliary function produces
-- an internal accumulator and an "held" output.
--
-- Applies a function point-wise, using the last known 'Just' output to form
-- the output, and next input accumulator. If the output is 'Nothing', the last
-- known accumulators are used. This creates a well-formed loop based on a
-- pure, auxiliary function.
sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f c_init b_init = SF {sfTF = tf0}
    where
        tf0 a0 = case f c_init a0 of
                     Nothing       -> (sfSScan f c_init b_init, b_init)
                     Just (c', b') -> (sfSScan f c' b', b')

-- Vim modeline
-- vim:set tabstop=8 expandtab:
