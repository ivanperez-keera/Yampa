-- | Minimal FRP core.
--
--   For documentation purposes only, to serve as a minimal FRP implementation.
--   Based on Antony Courtney's thesis "Modeling User Interfaces in a
--   Functional Language", page 48
--   (see https://www.antonycourtney.com/pubs/ac-thesis.pdf, page 61).
--
-- Notes:
--
-- - While 'time' is defined as "core", it is not a primitive in Yampa, and it
-- is actually defined as the 'integral' of @1@ over time.
--
-- - This does not include 'derivative'.
--
-- - This does not include parallel switching combinators (see
-- 'FRP.Yampa.Switches').
--
module FRP.Yampa.Core
    (
    -- * Signal function
      SF

    -- * Stateless combinators
    , iPre
    , arr
    , (>>>)
    , first

    -- * Stateful combinators
    , loop
      -- | Instantly loops an SF, making the second output also the second
      -- input, using the fix combinator. This introduces a instant loop;
      -- without delays, that may lead to an infinite loop.
    , integral

    -- ** Switching upon certain events
    , Event(..)
    , switch

    -- ** Time
    -- | Note: The function 'time' is actually the 'integral' of @1@ over time.
    -- So, it's not really necessary.
    , Time
    , time
    )
   where

import FRP.Yampa
