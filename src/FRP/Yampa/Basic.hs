{-# LANGUAGE GADTs, Rank2Types, CPP         #-}

-- Module      :  FRP.Yampa.Basic
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)

-- | Defines basic signal functions, and elementary ways of altering them.
--
-- This module defines very basic ways of creating and modifying signal
-- functions. In particular, it defines ways of creating constant output
-- producing SFs, and SFs that just pass the signal through unmodified.
--
-- It also defines ways of altering the input and the output signal only
-- by inserting one value in the signal, or by transforming it.
module FRP.Yampa.Basic (

    -- * Basic signal functions
    identity,           -- :: SF a a
    constant,           -- :: b -> SF a b

    -- ** Initialization
    (-->),              -- :: b -> SF a b -> SF a b,            infixr 0
    (>--),              -- :: a -> SF a b -> SF a b,            infixr 0
    (-=>),              -- :: (b -> b) -> SF a b -> SF a b      infixr 0
    (>=-),              -- :: (a -> a) -> SF a b -> SF a b      infixr 0
    initially           -- :: a -> SF a a

  ) where


import FRP.Yampa.InternalCore (SF(..), sfConst, sfId)

infixr 0 -->, >--, -=>, >=-

------------------------------------------------------------------------------
-- Basic signal functions
------------------------------------------------------------------------------

-- | Identity: identity = arr id
--
-- Using 'identity' is preferred over lifting id, since the arrow combinators
-- know how to optimise certain networks based on the transformations being
-- applied.
identity :: SF a a
identity = SF {sfTF = \a -> (sfId, a)}

{-# ANN constant "HLint: ignore Use const" #-}
-- | Identity: constant b = arr (const b)
--
-- Using 'constant' is preferred over lifting const, since the arrow combinators
-- know how to optimise certain networks based on the transformations being
-- applied.
constant :: b -> SF a b
constant b = SF {sfTF = \_ -> (sfConst b, b)}

------------------------------------------------------------------------------
-- Initialization
------------------------------------------------------------------------------

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(-->) :: b -> SF a b -> SF a b
b0 --> (SF {sfTF = tf10}) = SF {sfTF = \a0 -> (fst (tf10 a0), b0)}

-- | Input initialization operator.
--
-- The input at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(>--) :: a -> SF a b -> SF a b
a0 >-- (SF {sfTF = tf10}) = SF {sfTF = \_ -> tf10 a0}


-- | Transform initial output value.
--
-- Applies a transformation 'f' only to the first output value at
-- time zero.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF {sfTF = tf10}) =
    SF {sfTF = \a0 -> let (sf1, b0) = tf10 a0 in (sf1, f b0)}


-- | Transform initial input value.
--
-- Applies a transformation 'f' only to the first input value at
-- time zero.
{-# ANN (>=-) "HLint: ignore Avoid lambda" #-}
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF {sfTF = tf10}) = SF {sfTF = \a0 -> tf10 (f a0)}

-- | Override initial value of input signal.
initially :: a -> SF a a
initially = (--> identity)
