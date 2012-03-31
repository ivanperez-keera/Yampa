{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Task
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Task abstraction on top of signal transformers.
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Task (
    Task,
    mkTask,	-- :: SF a (b, Event c) -> Task a b c
    runTask,	-- :: Task a b c -> SF a (Either b c)	-- Might change.
    runTask_,	-- :: Task a b c -> SF a b
    taskToSF,	-- :: Task a b c -> SF a (b, Event c)	-- Might change.
    constT,	-- :: b -> Task a b c
    sleepT, 	-- :: Time -> b -> Task a b ()
    snapT, 	-- :: Task a b a
    timeOut, 	-- :: Task a b c -> Time -> Task a b (Maybe c)
    abortWhen, 	-- :: Task a b c -> SF a (Event d) -> Task a b (Either c d)
    repeatUntil,-- :: Monad m => m a -> (a -> Bool) -> m a
    for, 	-- :: Monad m => a -> (a -> a) -> (a -> Bool) -> m b -> m ()
    forAll, 	-- :: Monad m => [a] -> (a -> m b) -> m ()
    forEver 	-- :: Monad m => m a -> m b
) where

import FRP.Yampa
import FRP.Yampa.Utilities (snap)
import FRP.Yampa.Diagnostics

infixl 0 `timeOut`, `abortWhen`, `repeatUntil`


------------------------------------------------------------------------------
-- The Task type
------------------------------------------------------------------------------

-- CPS-based representation allowing a termination to be detected.
-- (Note the rank 2 polymorphic type!)
-- The representation can be changed if necessary, but the Monad laws
-- follow trivially in this case.
newtype Task a b c =
    Task (forall d . (c -> SF a (Either b d)) -> SF a (Either b d))


unTask :: Task a b c -> ((c -> SF a (Either b d)) -> SF a (Either b d))
unTask (Task f) = f


mkTask :: SF a (b, Event c) -> Task a b c
mkTask st = Task (switch (st >>> first (arr Left)))


-- "Runs" a task (unusually bad name?). The output from the resulting
-- signal transformer is tagged with Left while the underlying task is
-- running. Once the task has terminated, the output goes constant with
-- the value Right x, where x is the value of the terminating event.
runTask :: Task a b c -> SF a (Either b c)
runTask tk = (unTask tk) (\c -> constant (Right c))


-- Runs a task. The output becomes undefined once the underlying task has
-- terminated. Convenient e.g. for tasks which are known not to terminate.
runTask_ :: Task a b c -> SF a b
runTask_ tk = runTask tk
              >>> arr (either id (usrErr "AFRPTask" "runTask_"
                                         "Task terminated!"))


-- Seems as if the following is convenient after all. Suitable name???
-- Maybe that implies a representation change for Tasks?
-- Law: mkTask (taskToSF task) = task (but not (quite) vice versa.)
taskToSF :: Task a b c -> SF a (b, Event c)
taskToSF tk = runTask tk
	      >>> (arr (either id ((usrErr "AFRPTask" "runTask_"
                                           "Task terminated!")))
		   &&& edgeBy isEdge (Left undefined))
    where
        isEdge (Left _)  (Left _)  = Nothing
	isEdge (Left _)  (Right c) = Just c
	isEdge (Right _) (Right _) = Nothing
	isEdge (Right _) (Left _)  = Nothing


------------------------------------------------------------------------------
-- Monad instance
------------------------------------------------------------------------------

instance Monad (Task a b) where
    tk >>= f = Task (\k -> (unTask tk) (\c -> unTask (f c) k))
    return x = Task (\k -> k x)

{-
Let's check the monad laws:

    t >>= return
    = \k -> t (\c -> return c k)
    = \k -> t (\c -> (\x -> \k -> k x) c k)
    = \k -> t (\c -> (\x -> \k' -> k' x) c k)
    = \k -> t (\c -> k c)
    = \k -> t k
    = t
    QED

    return x >>= f
    = \k -> (return x) (\c -> f c k)
    = \k -> (\k -> k x) (\c -> f c k)
    = \k -> (\k' -> k' x) (\c -> f c k)
    = \k -> (\c -> f c k) x
    = \k -> f x k
    = f x
    QED

    (t >>= f) >>= g
    = \k -> (t >>= f) (\c -> g c k)
    = \k -> (\k' -> t (\c' -> f c' k')) (\c -> g c k)
    = \k -> t (\c' -> f c' (\c -> g c k))
    = \k -> t (\c' -> (\x -> \k' -> f x (\c -> g c k')) c' k)
    = \k -> t (\c' -> (\x -> f x >>= g) c' k)
    = t >>= (\x -> f x >>= g)
    QED

No surprises (obviously, since this is essentially just the CPS monad).
-}


------------------------------------------------------------------------------
-- Basic tasks
------------------------------------------------------------------------------

-- Non-terminating task with constant output b.
constT :: b -> Task a b c
constT b = mkTask (constant b &&& never)


-- "Sleeps" for t seconds with constant output b.
sleepT :: Time -> b -> Task a b ()
sleepT t b = mkTask (constant b &&& after t ())


-- Takes a "snapshot" of the input and terminates immediately with the input
-- value as the result. No time passes; law:
--
--    snapT >> snapT = snapT
--
snapT :: Task a b a
snapT = mkTask (constant (intErr "AFRPTask" "snapT" "Bad switch?") &&& snap)


------------------------------------------------------------------------------
-- Basic tasks combinators
------------------------------------------------------------------------------

-- Impose a time out on a task.
timeOut :: Task a b c -> Time -> Task a b (Maybe c)
tk `timeOut` t = mkTask ((taskToSF tk &&& after t ()) >>> arr aux)
    where
        aux ((b, ec), et) = (b, (lMerge (fmap Just ec)
					(fmap (const Nothing) et)))


-- Run a "guarding" event source (SF a (Event b)) in parallel with a
-- (possibly non-terminating) task. The task will be aborted at the
-- first occurrence of the event source (if it has not terminated itself
-- before that). Useful for separating sequencing and termination concerns.
-- E.g. we can do something "useful", but in parallel watch for a (exceptional)
-- condition which should terminate that activity, whithout having to check
-- for that condition explicitly during each and every phase of the activity.
-- Example: tsk `abortWhen` lbp
abortWhen :: Task a b c -> SF a (Event d) -> Task a b (Either c d)
tk `abortWhen` est = mkTask ((taskToSF tk &&& est) >>> arr aux)
    where
        aux ((b, ec), ed) = (b, (lMerge (fmap Left ec) (fmap Right ed)))


------------------------------------------------------------------------------
-- Loops
------------------------------------------------------------------------------

-- These are general monadic combinators. Maybe they don't really belong here.

-- Repeat m until result satisfies the predicate p
repeatUntil :: Monad m => m a -> (a -> Bool) -> m a
m `repeatUntil` p = m >>= \x -> if not (p x) then repeatUntil m p else return x


-- C-style for-loop.
-- Example: for 0 (+1) (>=10) ...
for :: Monad m => a -> (a -> a) -> (a -> Bool) -> m b -> m ()
for i f p m = if p i then m >> for (f i) f p m else return ()


-- Perform the monadic operation for each element in the list.
forAll :: Monad m => [a] -> (a -> m b) -> m ()
forAll = flip mapM_


-- Repeat m for ever.
forEver :: Monad m => m a -> m b
forEver m = m >> forEver m


-- Alternatives/other potentially useful signatures:
-- until :: a -> (a -> M a) -> (a -> Bool) -> M a
-- for: a -> b -> (a -> b -> a) -> (a -> b -> Bool) -> (a -> b -> M b) -> M b
-- while??? It could be:
-- while :: a -> (a -> Bool) -> (a -> M a) -> M a


------------------------------------------------------------------------------
-- Monad transformers?
------------------------------------------------------------------------------

-- What about monad transformers if we want to compose this monad with
-- other capabilities???
