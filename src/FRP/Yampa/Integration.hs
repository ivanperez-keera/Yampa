-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Integration
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Integration and derivation of input signals.
--
-- In continuous time, these primitives define SFs that integrate/derive the
-- input signal. Since this is subject to the sampling resolution, simple
-- versions are implemented (like the rectangle rule for the integral).
--
-- In discrete time, all we do is count the number of events.
--
-- The combinator 'iterFrom' gives enough flexibility to program your own
-- leak-free integration and derivation SFs.
-----------------------------------------------------------------------------------------

module FRP.Yampa.Integration (

    -- * Integration
    integral,           -- :: VectorSpace a s => SF a a
    imIntegral,         -- :: VectorSpace a s => a -> SF a a
    impulseIntegral,    -- :: VectorSpace a k => SF (a, Event a) a
    count,              -- :: Integral b => SF (Event a) (Event b)

    -- * Differentiation
    derivative,         -- :: VectorSpace a s => SF a a         -- Crude!
    iterFrom            -- :: (a -> a -> DTime -> b -> b) -> b -> SF a b

) where

import Control.Arrow
import FRP.Yampa.Event
import FRP.Yampa.Hybrid
import FRP.Yampa.InternalCore (SF(..), SF'(..), DTime)
import FRP.Yampa.VectorSpace

------------------------------------------------------------------------------
-- Integration and differentiation
------------------------------------------------------------------------------

-- | Integration using the rectangle rule.
{-# INLINE integral #-}
integral :: VectorSpace a s => SF a a
integral = SF {sfTF = tf0}
    where
        tf0 a0 = (integralAux igrl0 a0, igrl0)

        igrl0  = zeroVector

        integralAux igrl a_prev = SF' tf -- True
            where
                tf dt a = (integralAux igrl' a, igrl')
                    where
                       igrl' = igrl ^+^ realToFrac dt *^ a_prev


-- | \"Immediate\" integration (using the function's value at the current time)
imIntegral :: VectorSpace a s => a -> SF a a
imIntegral = ((\ _ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`)

-- | Integrate using an auxiliary function that takes the current and the last
--   input, the time between those samples, and the last output, and returns a
--   new output.
iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
f `iterFrom` b = SF (iterAux b)
    where
        iterAux b a = (SF' (\ dt a' -> iterAux (f a a' dt b) a'), b)

-- | A very crude version of a derivative. It simply divides the
--   value difference by the time difference. Use at your own risk.
derivative :: VectorSpace a s => SF a a
derivative = SF {sfTF = tf0}
    where
        tf0 a0 = (derivativeAux a0, zeroVector)

        derivativeAux a_prev = SF' tf -- True
            where
                tf dt a = (derivativeAux a, (a ^-^ a_prev) ^/ realToFrac dt)

-- | Integrate the first input signal and add the /discrete/ accumulation (sum)
--   of the second, discrete, input signal.
impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

-- | Count the occurrences of input events.
--
-- >>> embed count (deltaEncode 1 [Event 'a', NoEvent, Event 'b'])
-- [Event 1,NoEvent,Event 2]
count :: Integral b => SF (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0


-- Vim modeline
-- vim:set tabstop=8 expandtab:
