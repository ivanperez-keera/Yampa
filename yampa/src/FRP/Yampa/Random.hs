-- |
-- Module      :  FRP.Yampa.Random
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Signals and signal functions with noise and randomness.
--
-- The Random number generators are re-exported from "System.Random".
module FRP.Yampa.Random
    (
      -- * Random number generators
      RandomGen(..)
    , Random(..)

      -- * Noise, random signals, and stochastic event sources
    , noise
    , noiseR
    , occasionally
    )
  where

import System.Random (Random (..), RandomGen (..))

import FRP.Yampa.Diagnostics
import FRP.Yampa.Event
import FRP.Yampa.InternalCore (SF (..), SF' (..), Time)

-- * Noise (i.e. random signal generators) and stochastic processes

-- | Noise (random signal) with default range for type in question;
-- based on "randoms".
noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)

-- | Noise (random signal) with specified range; based on "randomRs".
noiseR :: (RandomGen g, Random b) => (b,b) -> g -> SF a b
noiseR range g0 = streamToSF (randomRs range g0)

streamToSF :: [b] -> SF a b
streamToSF []     = intErr "Yampa" "streamToSF" "Empty list!"
streamToSF (b:bs) = SF {sfTF = tf0}
  where
    tf0 _ = (stsfAux bs, b)

    stsfAux []     = intErr "Yampa" "streamToSF" "Empty list!"
    -- Invarying since stsfAux [] is an error.
    stsfAux (b:bs) = SF' tf -- True
      where
        tf _ _ = (stsfAux bs, b)

-- | Stochastic event source with events occurring on average once every t_avg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.

occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally g t_avg x | t_avg > 0 = SF {sfTF = tf0}
                       | otherwise = usrErr "Yampa" "occasionally"
                                            "Non-positive average interval."
  where
    -- Generally, if events occur with an average frequency of f, the
    -- probability of at least one event occurring in an interval of t is given
    -- by (1 - exp (-f*t)). The goal in the following is to decide whether at
    -- least one event occurred in the interval of size dt preceding the
    -- current sample point. For the first point, we can think of the preceding
    -- interval as being 0, implying no probability of an event occurring.

    tf0 _ = (occAux (randoms g :: [Time]), NoEvent)

    occAux [] = undefined
    occAux (r:rs) = SF' tf -- True
      where
        tf dt _ = let p = 1 - exp (-(dt/t_avg)) -- Probability for at least one
                                                -- event.
                  in (occAux rs, if r < p then Event x else NoEvent)
