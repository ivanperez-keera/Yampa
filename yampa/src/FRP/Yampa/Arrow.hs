-- |
-- Module      : FRP.Yampa.Arrow
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : portable
--
-- Arrow helper functions.
module FRP.Yampa.Arrow
    (
      -- * Arrow plumbing aids
      dup

      -- * Liftings
    , arr2
    , arr3
    , arr4
    , arr5
    )
  where

-- External imports
import Control.Arrow (Arrow, arr)

-- * Arrow plumbing aids

-- | Duplicate an input.
dup :: a -> (a, a)
dup x = (x, x)

-- * Liftings

-- | Lift a binary function onto an arrow.
arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry

{-# DEPRECATED arr3 "The function arr3 is deprecated in Yampa 0.15 and will be removed in future versions." #-}
-- | Lift a 3-ary function onto an arrow.
arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d

{-# DEPRECATED arr4 "The function arr4 is deprecated in Yampa 0.15 and will be removed in future versions." #-}
-- | Lift a 4-ary function onto an arrow.
arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e

{-# DEPRECATED arr5 "The function arr5 is deprecated in Yampa 0.15 and will be removed in future versions." #-}
-- | Lift a 5-ary function onto an arrow.
arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f
