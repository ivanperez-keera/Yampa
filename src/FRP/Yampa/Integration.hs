{-# LANGUAGE GADTs, Rank2Types, CPP #-}
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
-----------------------------------------------------------------------------------------

module FRP.Yampa.Integration (

    -- * Integration
    integral,           -- :: VectorSpace a s => SF a a
    imIntegral,         -- :: VectorSpace a s => a -> SF a a
    impulseIntegral,    -- :: VectorSpace a k => SF (a, Event a) a
    count,              -- :: Integral b => SF (Event a) (Event b)

    -- * Differentiation
    derivative          -- :: VectorSpace a s => SF a a         -- Crude!


    -- Temporarily hidden, but will eventually be made public.
    -- iterFrom,           -- :: (a -> a -> DTime -> b -> b) -> b -> SF a b

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

impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

count :: Integral b => SF (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0


-- Vim modeline
-- vim:set tabstop=8 expandtab:
