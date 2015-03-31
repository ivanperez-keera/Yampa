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

    -- ** Time (NOTE: integral 1 over time. Not really necessary.)
    , Time
    , time
    )
   where

import FRP.Yampa
