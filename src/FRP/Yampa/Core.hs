-- | Core of Yampa, as defined in Antony Courtney's thesis
-- [https://www.antonycourtney.com/pubs/ac-thesis.pdf, page 61].
--
-- Notes:
--
-- - While 'time' is defined as "core", it is not a primitive in Yampa, and it
-- is actually defined as the 'integral' of 1 over time.
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
    , integral

    -- ** Switching upon certain events
    , Event(..)
    , switch

    -- ** Time
    , Time
    , time
    )
   where

import FRP.Yampa
