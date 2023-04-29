-- |
-- Module      : FRP.Yampa.Loop
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
--
-- Portability : non-portable -GHC extensions-
--
-- Well-initialised loops.
module FRP.Yampa.Loop
    (
      -- * Loops with guaranteed well-defined feedback
      loopPre
    , loopIntegral
    )
  where

-- External imports
import Control.Arrow    (loop, second, (>>>))
import Data.VectorSpace (VectorSpace)

-- Internal imports
import FRP.Yampa.Delays       (iPre)
import FRP.Yampa.Integration  (integral)
import FRP.Yampa.InternalCore (SF)

-- * Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: c -> SF (a, c) (b, c) -> SF a b
loopPre cInit sf = loop (second (iPre cInit) >>> sf)

-- | Loop by integrating the second value in the pair and feeding the result
-- back. Because the integral at time 0 is zero, this is always well defined.
loopIntegral :: (Fractional s, VectorSpace c s) => SF (a, c) (b, c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)
