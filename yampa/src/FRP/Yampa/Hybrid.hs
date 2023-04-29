-- |
-- Module      : FRP.Yampa.Hybrid
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
-- Discrete to continuous-time signal functions.
module FRP.Yampa.Hybrid
    (
      -- * Wave-form generation
      hold
    , dHold
    , trackAndHold
    , dTrackAndHold

      -- * Accumulators
    , accum
    , accumHold
    , dAccumHold
    , accumBy
    , accumHoldBy
    , dAccumHoldBy
    , accumFilter
    )
  where

-- External imports
import Control.Arrow (arr, (>>>))

-- Internal imports
import FRP.Yampa.Delays       (iPre)
import FRP.Yampa.Event        (Event (..))
import FRP.Yampa.InternalCore (SF, epPrim)

-- * Wave-form generation

-- | Zero-order hold.
--
-- Converts a discrete-time signal into a continuous-time signal, by holding the
-- last value until it changes in the input signal. The given parameter may be
-- used for time zero, and until the first event occurs in the input signal, so
-- hold is always well-initialized.
--
-- >>> embed (hold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,2,2,3,3]
hold :: a -> SF (Event a) a
hold aInit = epPrim f () aInit
  where
    f _ a = ((), a, a)

-- | Zero-order hold with a delay.
--
-- Converts a discrete-time signal into a continuous-time signal, by holding the
-- last value until it changes in the input signal. The given parameter is used
-- for time zero (until the first event occurs in the input signal), so 'dHold'
-- shifts the discrete input by an infinitesimal delay.
--
-- >>> embed (dHold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,1,2,2,3]
dHold :: a -> SF (Event a) a
dHold a0 = hold a0 >>> iPre a0

-- | Tracks input signal when available, holding the last value when the input
-- is 'Nothing'.
--
-- This behaves similarly to 'hold', but there is a conceptual difference, as it
-- takes a signal of input @Maybe a@ (for some @a@) and not @Event@.
--
-- >>> embed (trackAndHold 1) (deltaEncode 0.1 [Nothing, Nothing, Just 2, Nothing, Just 3, Nothing])
-- [1,1,2,2,3,3]
trackAndHold :: a -> SF (Maybe a) a
trackAndHold aInit = arr (maybe NoEvent Event) >>> hold aInit

-- | Tracks input signal when available, holding the last value when the input
-- is 'Nothing', with a delay.
--
-- This behaves similarly to 'hold', but there is a conceptual difference, as it
-- takes a signal of input @Maybe a@ (for some @a@) and not @Event@.
--
-- >>> embed (dTrackAndHold 1) (deltaEncode 0.1 [Nothing, Nothing, Just 2, Nothing, Just 3, Nothing])
-- [1,1,1,2,2,3]

dTrackAndHold :: a -> SF (Maybe a) a
dTrackAndHold aInit = trackAndHold aInit >>> iPre aInit

-- * Accumulators

-- | Given an initial value in an accumulator, it returns a signal function that
-- processes an event carrying transformation functions. Every time an 'Event'
-- is received, the function inside it is applied to the accumulator, whose new
-- value is outputted in an 'Event'.
--
accum :: a -> SF (Event (a -> a)) (Event a)
accum aInit = epPrim f aInit NoEvent
  where
    f a g = (a', Event a', NoEvent) -- Accumulator, output if Event, output if
                                    -- no event
      where
        a' = g a

-- | Zero-order hold accumulator (always produces the last outputted value until
-- an event arrives).
accumHold :: a -> SF (Event (a -> a)) a
accumHold aInit = epPrim f aInit aInit
  where
    f a g = (a', a', a') -- Accumulator, output if Event, output if no event
      where
        a' = g a

-- | Zero-order hold accumulator with delayed initialization (always produces
-- the last outputted value until an event arrives, but the very initial output
-- is always the given accumulator).
dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold aInit = accumHold aInit >>> iPre aInit

-- | Accumulator parameterized by the accumulation function.
accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
accumBy g bInit = epPrim f bInit NoEvent
  where
    f b a = (b', Event b', NoEvent)
      where
        b' = g b a

-- | Zero-order hold accumulator parameterized by the accumulation function.
accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy g bInit = epPrim f bInit bInit
  where
    f b a = (b', b', b')
      where
        b' = g b a

-- | Zero-order hold accumulator parameterized by the accumulation function with
-- delayed initialization (initial output sample is always the given
-- accumulator).
dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f aInit = accumHoldBy f aInit >>> iPre aInit

-- | Accumulator parameterized by the accumulator function with filtering,
-- possibly discarding some of the input events based on whether the second
-- component of the result of applying the accumulation function is 'Nothing' or
-- 'Just' x for some x.
accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter g cInit = epPrim f cInit NoEvent
  where
    f c a = case g c a of
              (c', Nothing) -> (c', NoEvent, NoEvent)
              (c', Just b)  -> (c', Event b, NoEvent)
