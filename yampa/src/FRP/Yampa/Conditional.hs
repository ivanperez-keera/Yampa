-- | Apply SFs only under certain conditions.
module FRP.Yampa.Conditional (
    provided  -- :: (a -> Bool) -> SF a b -> SF a b -> SF a b
  , pause     -- :: b -> SF a b -> SF a Bool -> SF a b

  ) where

import Control.Arrow
import FRP.Yampa.Basic
import FRP.Yampa.InternalCore (SF(..), SF'(..), sfTF', Transition)
import FRP.Yampa.EventS
import FRP.Yampa.Switches

-- * Guards and automata-oriented combinators

-- | Runs a signal function only when a given predicate is satisfied, otherwise
-- runs the other signal function.
--
-- This is similar to 'ArrowChoice', except that this resets the SFs after each
-- transition.
-- 
-- For example, the following integrates the incoming input numbers, using one
-- integral if the numbers are even, and another if the input numbers are odd.
-- Note how, every time we "switch", the old value of the integral is
-- discarded.
-- 
-- >>> embed (provided (even . round) integral integral) (deltaEncode 1 [1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2 :: Double])
-- [0.0,1.0,2.0,0.0,2.0,4.0,0.0,1.0,2.0,0.0,2.0,4.0]

provided :: (a -> Bool) -> SF a b -> SF a b -> SF a b
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
      if p a0 then stt else stf
    where
      stt = switch (sft &&& (not . p ^>> edge)) (const stf)
      stf = switch (sff &&& (p ^>> edge)) (const stt)

-- * Variable pause

-- | Given a value in an accumulator (b), a predicate signal function (sfC),
--   and a second signal function (sf), pause will produce the accumulator b
--   if sfC input is True, and will transform the signal using sf otherwise.
--   It acts as a pause with an accumulator for the moments when the
--   transformation is paused.
pause :: b -> SF a Bool -> SF a b -> SF a b
pause b_init (SF { sfTF = tfP}) (SF {sfTF = tf10}) = SF {sfTF = tf0}
 where
       -- Initial transformation (no time delta):
       -- If the condition is True, return the accumulator b_init)
       -- Otherwise transform the input normally and recurse.
       tf0 a0 = case tfP a0 of
                 (c, True)  -> (pauseInit b_init tf10 c, b_init)
                 (c, False) -> let (k, b0) = tf10 a0
                               in (pause' b0 k c, b0)

       -- Similar deal, but with a time delta
       pauseInit :: b -> (a -> Transition a b) -> SF' a Bool -> SF' a b
       pauseInit b_init' tf10' c = SF' tf0'
         where tf0' dt a =
                case (sfTF' c) dt a of
                  (c', True)  -> (pauseInit b_init' tf10' c', b_init')
                  (c', False) -> let (k, b0) = tf10' a
                                 in (pause' b0 k c', b0)

       -- Very same deal (almost alpha-renameable)
       pause' :: b -> SF' a b -> SF' a Bool -> SF' a b
       pause' b_init' tf10' tfP' = SF' tf0'
         where tf0' dt a =
                 case (sfTF' tfP') dt a of
                   (tfP'', True) -> (pause' b_init' tf10' tfP'', b_init')
                   (tfP'', False) -> let (tf10'', b0') = (sfTF' tf10') dt a
                                     in (pause' b0' tf10'' tfP'', b0')


