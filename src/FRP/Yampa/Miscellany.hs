-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Miscellany
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Collection of entities that really should be part of Haskell base.
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Miscellany (
    -- * Arrow plumbing aids
    dup,        -- :: a -> (a,a)

    -- * Liftings
    arr2,       -- :: Arrow a => (b->c->d) -> a (b,c) d
    arr3,       -- :: Arrow a => (b->c->d->e) -> a (b,c,d) e
    arr4,       -- :: Arrow a => (b->c->d->e->f) -> a (b,c,d,e) f
    arr5,       -- :: Arrow a => (b->c->d->e->f->g) -> a (b,c,d,e,f) g
    lift0,      -- :: Arrow a => c -> a b c
    lift1,      -- :: Arrow a => (c->d) -> (a b c->a b d)
    lift2,      -- :: Arrow a => (c->d->e) -> (a b c->a b d->a b e)
    lift3,      -- :: Arrow a => (c->d->e->f) -> (a b c-> ... ->a b f)
    lift4,      -- :: Arrow a => (c->d->e->f->g) -> (a b c->...->a b g)
    lift5,      -- :: Arrow a => (c->d->e->f->g->h)->(a b c->...a b h)
) where

import Control.Arrow

------------------------------------------------------------------------------
-- Arrow plumbing aids

-- | Duplicate an input.
dup :: a -> (a,a)
dup x = (x,x)

-- * Arrows

-- ** Liftings

-- | Lift a binary function onto an arrow
arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry

-- | Lift a 3-ary function onto an arrow
arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d

-- | Lift a 4-ary function onto an arrow
arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e

-- | Lift a 5-ary function onto an arrow
arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f

-- | Lift an 0-ary function onto an arrow
--
-- If there was an @arr0@ function, this would be a synonym.
lift0 :: Arrow a => c -> a b c
lift0 c = arr (const  c)

-- | Lift a function into a function between arrows.
lift1 :: Arrow a => (c -> d) -> (a b c -> a b d)
lift1 f = \a -> a >>> arr f

-- | Lift a binary function into a function between arrows.
lift2 :: Arrow a => (c -> d -> e) -> (a b c -> a b d -> a b e)
lift2 f = \a1 a2 -> a1 &&& a2 >>> arr2 f

-- | Lift a 3-ary function into a function between arrows.
lift3 :: Arrow a => (c -> d -> e -> f) -> (a b c -> a b d -> a b e -> a b f)
lift3 f = \a1 a2 a3 -> (lift2 f) a1 a2 &&& a3 >>> arr2 ($)

-- | Lift a 4-ary function into a function between arrows.
lift4 :: Arrow a => (c->d->e->f->g) -> (a b c->a b d->a b e->a b f->a b g)
lift4 f = \a1 a2 a3 a4 -> (lift3 f) a1 a2 a3 &&& a4 >>> arr2 ($)

-- | Lift a 5-ary function into a function between arrows.
lift5 :: Arrow a =>
    (c->d->e->f->g->h) -> (a b c->a b d->a b e->a b f->a b g->a b h)
lift5 f = \a1 a2 a3 a4 a5 ->(lift4 f) a1 a2 a3 a4 &&& a5 >>> arr2 ($)
