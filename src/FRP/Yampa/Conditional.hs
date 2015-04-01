module FRP.Yampa.Conditional (
    -- Guards and automata-oriented combinators
    provided        -- :: (a -> Bool) -> SF a b -> SF a b -> SF a b
  ) where

import Control.Arrow
import FRP.Yampa.Basic
import FRP.Yampa.Core
import FRP.Yampa.EventS

------------------------------------------------------------------------------
-- Guards and automata-oriented combinators
------------------------------------------------------------------------------


-- Runs sft only when the predicate p is satisfied, otherwise runs sff.
provided :: (a -> Bool) -> SF a b -> SF a b -> SF a b
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
      if p a0 then stt else stf
    where
      stt = switch (sft &&& (not . p ^>> edge)) (const stf)
      stf = switch (sff &&& (p ^>> edge)) (const stt)
