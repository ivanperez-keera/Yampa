-- |
-- Copyright  : (c) Ivan Perez, 2017-2022
-- License    : BSD-style (see the LICENSE file in the distribution)
-- Maintainer : ivan.perez@keera.co.uk
--
-- Past-time Linear Temporal Logics based on SFs.
--
-- This module contains a definition of ptLTL with prev/last on top of Signal
-- Functions.
--
-- The difference between the future time and the past time LTL is that the
-- former needs a trace for evaluation, and the latter can be embedded into a
-- signal function network without additional support for evaluation.
module FRP.Yampa.LTLPast where

-- External imports
import FRP.Yampa (Event (..), SF, arr, iPre, loopPre, switch, (>>>))

-- | True if both inputs are True.
andSF :: SF (Bool, Bool) Bool
andSF = arr (uncurry (&&))

-- | True if either or both inputs are True.
orSF :: SF (Bool, Bool) Bool
orSF = arr (uncurry (||))

-- | True if the input signal is False.
notSF :: SF Bool Bool
notSF = arr not

-- | True if the first signal is False or the second one is True.
impliesSF :: SF (Bool, Bool) Bool
impliesSF = arr $ \(i, p) -> not i || p

-- | True a a time if the input signal has been always True so far.
sofarSF :: SF Bool Bool
sofarSF = loopPre True $ arr $ \(n, o) -> let n' = o && n in (n', n')

-- | True at a time if the input signal has ever been True before.
everSF :: SF Bool Bool
everSF = loopPre False $ arr $ \(n, o) -> let n' = o || n in (n', n')

-- | True if the signal was True in the last sample. False at time zero.
lastSF :: SF Bool Bool
lastSF = iPre False

-- | Weak Until. True if the first signal is True until the second becomes
-- True, if ever.
untilSF :: SF (Bool, Bool) Bool
untilSF = switch
  (loopPre True $ arr (\((i, u), o) ->
     let n = o && i
     in ((n, if o && u then Event () else NoEvent), n)))
  (\_ -> arr snd >>> sofarSF)
