{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Random
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Random (
    RandomGen(..),
    Random(..),

-- * Noise (random signal) sources and stochastic event sources
    noise,              -- :: noise :: (RandomGen g, Random b) =>
                        --        g -> SF a b
    noiseR,             -- :: noise :: (RandomGen g, Random b) =>
                        --        (b,b) -> g -> SF a b
    occasionally,       -- :: RandomGen g => g -> Time -> b -> SF a (Event b)

) where

import System.Random (RandomGen(..), Random(..))

import FRP.Yampa.InternalCore (SF(..), SF'(..), Time)
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event

------------------------------------------------------------------------------
-- Noise (i.e. random signal generators) and stochastic processes
------------------------------------------------------------------------------

-- | Noise (random signal) with default range for type in question;
-- based on "randoms".
noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)


-- | Noise (random signal) with specified range; based on "randomRs".
noiseR :: (RandomGen g, Random b) => (b,b) -> g -> SF a b
noiseR range g0 = streamToSF (randomRs range g0)


-- Internal. Not very useful for other purposes since we do not have any
-- control over the intervals between each "sample". Or? A version with
-- time-stamped samples would be similar to embedSynch (applied to identity).
-- The list argument must be a stream (infinite list) at present.

streamToSF :: [b] -> SF a b
streamToSF []     = intErr "AFRP" "streamToSF" "Empty list!"
streamToSF (b:bs) = SF {sfTF = tf0}
    where
        tf0 _ = (stsfAux bs, b)

        stsfAux []     = intErr "AFRP" "streamToSF" "Empty list!"
        -- Invarying since stsfAux [] is an error.
        stsfAux (b:bs) = SF' tf -- True
            where
                tf _ _ = (stsfAux bs, b)

{- New def, untested:

streamToSF = sscan2 f
    where
        f []     _ = intErr "AFRP" "streamToSF" "Empty list!"
        f (b:bs) _ = (bs, b)

-}


-- | Stochastic event source with events occurring on average once every t_avg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.

-- !!! Maybe it would better to give a frequency? But like this to make
-- !!! consitent with "repeatedly".
occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally g t_avg x | t_avg > 0 = SF {sfTF = tf0}
                       | otherwise = usrErr "AFRP" "occasionally"
                                            "Non-positive average interval."
    where
        -- Generally, if events occur with an average frequency of f, the
        -- probability of at least one event occurring in an interval of t
        -- is given by (1 - exp (-f*t)). The goal in the following is to
        -- decide whether at least one event occurred in the interval of size
        -- dt preceding the current sample point. For the first point,
        -- we can think of the preceding interval as being 0, implying
        -- no probability of an event occurring.

    tf0 _ = (occAux (randoms g :: [Time]), NoEvent)

    occAux [] = undefined
    occAux (r:rs) = SF' tf -- True
        where
        tf dt _ = let p = 1 - exp (-(dt/t_avg)) -- Probability for at least one event.
                  in (occAux rs, if r < p then Event x else NoEvent)


-- Vim modeline
-- vim:set tabstop=8 expandtab:
