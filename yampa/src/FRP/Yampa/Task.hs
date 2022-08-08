{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Module      :  FRP.Yampa.Task
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Task abstraction on top of signal transformers.
module FRP.Yampa.Task
    ( Task
    , mkTask
    , runTask
    , runTask_
    , taskToSF
    , constT
    , sleepT
    , snapT
    , timeOut
    , abortWhen
    )
  where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif

import FRP.Yampa.Basic        (constant)
import FRP.Yampa.Diagnostics  (intErr, usrErr)
import FRP.Yampa.Event        (Event, lMerge)
import FRP.Yampa.EventS       (after, edgeBy, never, snap)
import FRP.Yampa.InternalCore (SF, Time, arr, first, (&&&), (>>>))
import FRP.Yampa.Switches     (switch)

infixl 0 `timeOut`, `abortWhen`

-- * The Task type

-- | A task is a partially SF that may terminate with a result.

newtype Task a b c =
  -- CPS-based representation allowing termination to be detected.
  -- (Note the rank 2 polymorphic type!)
  -- The representation can be changed if necessary, but the Monad laws
  -- follow trivially in this case.
  Task (forall d . (c -> SF a (Either b d)) -> SF a (Either b d))

unTask :: Task a b c -> ((c -> SF a (Either b d)) -> SF a (Either b d))
unTask (Task f) = f

-- | Creates a 'Task' from an SF that returns, as a second output, an 'Event'
-- when the SF terminates. See 'switch'.
mkTask :: SF a (b, Event c) -> Task a b c
mkTask st = Task (switch (st >>> first (arr Left)))

-- | Runs a task.
--
-- The output from the resulting signal transformer is tagged with Left while
-- the underlying task is running. Once the task has terminated, the output
-- goes constant with the value Right x, where x is the value of the
-- terminating event.

-- Check name.
runTask :: Task a b c -> SF a (Either b c)
runTask tk = (unTask tk) (constant . Right)

-- | Runs a task that never terminates.
--
-- The output becomes undefined once the underlying task has terminated.
--
-- Convenience function for tasks which are known not to terminate.
runTask_ :: Task a b c -> SF a b
runTask_ tk = runTask tk
              >>> arr (either id (usrErr "YampaTask" "runTask_"
                                         "Task terminated!"))

-- | Creates an SF that represents an SF and produces an event
-- when the task terminates, and otherwise produces just an output.
taskToSF :: Task a b c -> SF a (b, Event c)
taskToSF tk = runTask tk
              >>> (arr (either id (usrErr "YampaTask" "runTask_"
                                          "Task terminated!"))
                   &&& edgeBy isEdge (Left undefined))
  where
    isEdge (Left _)  (Left _)  = Nothing
    isEdge (Left _)  (Right c) = Just c
    isEdge (Right _) (Right _) = Nothing
    isEdge (Right _) (Left _)  = Nothing

-- * Functor, Applicative and Monad instance

instance Functor (Task a b) where
  fmap f tk = Task (\k -> unTask tk (k . f))

instance Applicative (Task a b) where
  pure x  = Task (\k -> k x)
  f <*> v = Task (\k -> (unTask f) (\c -> unTask v (k . c)))

instance Monad (Task a b) where
  tk >>= f = Task (\k -> unTask tk (\c -> unTask (f c) k))
  return x = Task (\k -> k x)

-- Let's check the monad laws:
--
--   t >>= return
--   = \k -> t (\c -> return c k)
--   = \k -> t (\c -> (\x -> \k -> k x) c k)
--   = \k -> t (\c -> (\x -> \k' -> k' x) c k)
--   = \k -> t (\c -> k c)
--   = \k -> t k
--   = t
--   QED
--
--   return x >>= f
--   = \k -> (return x) (\c -> f c k)
--   = \k -> (\k -> k x) (\c -> f c k)
--   = \k -> (\k' -> k' x) (\c -> f c k)
--   = \k -> (\c -> f c k) x
--   = \k -> f x k
--   = f x
--   QED
--
--   (t >>= f) >>= g
--   = \k -> (t >>= f) (\c -> g c k)
--   = \k -> (\k' -> t (\c' -> f c' k')) (\c -> g c k)
--   = \k -> t (\c' -> f c' (\c -> g c k))
--   = \k -> t (\c' -> (\x -> \k' -> f x (\c -> g c k')) c' k)
--   = \k -> t (\c' -> (\x -> f x >>= g) c' k)
--   = t >>= (\x -> f x >>= g)
--   QED
--
-- No surprises (obviously, since this is essentially just the CPS monad).

-- * Basic tasks

-- | Non-terminating task with constant output b.
constT :: b -> Task a b c
constT b = mkTask (constant b &&& never)

-- | "Sleeps" for t seconds with constant output b.
sleepT :: Time -> b -> Task a b ()
sleepT t b = mkTask (constant b &&& after t ())

-- | Takes a "snapshot" of the input and terminates immediately with the input
-- value as the result.
--
-- No time passes; therefore, the following must hold:
--
-- @snapT >> snapT = snapT@

snapT :: Task a b a
snapT = mkTask (constant (intErr "YampaTask" "snapT" "Bad switch?") &&& snap)

-- * Basic tasks combinators

-- | Impose a time out on a task.
timeOut :: Task a b c -> Time -> Task a b (Maybe c)
tk `timeOut` t = mkTask ((taskToSF tk &&& after t ()) >>> arr aux)
  where
    aux ((b, ec), et) = (b, (lMerge (fmap Just ec) (fmap (const Nothing) et)))

-- | Run a "guarding" event source (SF a (Event b)) in parallel with a
-- (possibly non-terminating) task.
--
-- The task will be aborted at the first occurrence of the event source (if it
-- has not terminated itself before that).
--
-- Useful for separating sequencing and termination concerns.  E.g. we can do
-- something "useful", but in parallel watch for a (exceptional) condition
-- which should terminate that activity, without having to check for that
-- condition explicitly during each and every phase of the activity.
--
-- Example: @tsk `abortWhen` lbp@
abortWhen :: Task a b c -> SF a (Event d) -> Task a b (Either c d)
tk `abortWhen` est = mkTask ((taskToSF tk &&& est) >>> arr aux)
  where
    aux ((b, ec), ed) = (b, (lMerge (fmap Left ec) (fmap Right ed)))
