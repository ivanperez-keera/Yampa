-- |
-- Module      : FRP.Yampa.Scan
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
-- Simple, stateful signal processing.
--
-- Scanning implements elementary, step-based accumulating over signal functions
-- by means of an auxiliary function applied to each input and to an
-- accumulator. For comparison with other FRP libraries and with stream
-- processing abstractions, think of fold.
module FRP.Yampa.Scan
    ( sscan
    , sscanPrim
    )
  where

-- Internal imports
import FRP.Yampa.InternalCore (SF(..), sfSScan)

-- ** Simple, stateful signal processing

-- | Applies a function point-wise, using the last output as next input. This
-- creates a well-formed loop based on a pure, auxiliary function.
sscan :: (b -> a -> b) -> b -> SF a b
sscan f bInit = sscanPrim f' bInit bInit
  where
    f' b a = let b' = f b a in Just (b', b')

-- | Generic version of 'sscan', in which the auxiliary function produces an
-- internal accumulator and an "held" output.
--
-- Applies a function point-wise, using the last known 'Just' output to form the
-- output, and next input accumulator. If the output is 'Nothing', the last
-- known accumulators are used. This creates a well-formed loop based on a pure,
-- auxiliary function.
sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f cInit bInit = SF {sfTF = tf0}
  where
    tf0 a0 = case f cInit a0 of
               Nothing       -> (sfSScan f cInit bInit, bInit)
               Just (c', b') -> (sfSScan f c' b',       b')
