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
-- Collection of entities that really should be part
-- of the Haskell 98 prelude or simply have no better
-- home.
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Miscellany (
-- Reverse function composition
    ( # ),      -- :: (a -> b) -> (b -> c) -> (a -> c), infixl 9

-- Arrow plumbing aids
    dup,        -- :: a -> (a,a)

-- Maps over lists of pairs
    mapFst,     -- :: (a -> b) -> [(a,c)] -> [(b,c)]
    mapSnd,     -- :: (a -> b) -> [(c,a)] -> [(c,b)]

-- Generalized tuple selectors
    sel3_1, sel3_2, sel3_3,
    sel4_1, sel4_2, sel4_3, sel4_4,
    sel5_1, sel5_2, sel5_3, sel5_4, sel5_5,

-- Floating point utilities
    fDiv,       -- :: (RealFrac a, Integral b) => a -> a -> b
    fMod,       -- :: RealFrac a => a -> a -> a
    fDivMod,    -- :: (RealFrac a, Integral b) => a -> a -> (b, a)

-- Liftings
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

infixl 9 #
infixl 7 `fDiv`, `fMod`


------------------------------------------------------------------------------
-- Reverse function composition
------------------------------------------------------------------------------

-- !!! Reverse function composition should go.
-- !!! Better to use <<< and >>> for, respectively,
-- !!! function composition and reverse function composition.

{-# DEPRECATED (#) "Use Control.Arrow.(>>>) and Control.Arrow.(<<<)." #-}
( # ) :: (a -> b) -> (b -> c) -> (a -> c)
f # g = g . f


------------------------------------------------------------------------------
-- Arrow plumbing aids
------------------------------------------------------------------------------

dup :: a -> (a,a)
dup x = (x,x)

------------------------------------------------------------------------------
-- Maps over lists of pairs
------------------------------------------------------------------------------

{-# DEPRECATED mapFst "mapFst is not used by Yampa and will be removed from the next release" #-}
mapFst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFst f = map (\(x,y) -> (f x, y))

{-# DEPRECATED mapSnd "mapSnd is not used by Yampa and will be removed from the next release" #-}
mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f = map (\(x,y) -> (x, f y))


------------------------------------------------------------------------------
-- Generalized tuple selectors
------------------------------------------------------------------------------

{-# DEPRECATED sel3_1, sel3_2, sel3_3 "Use the tuple package instead." #-}
-- Triples
sel3_1 :: (a, b, c) -> a
sel3_1 (x,_,_) = x
sel3_2 :: (a, b, c) -> b
sel3_2 (_,x,_) = x
sel3_3 :: (a, b, c) -> c
sel3_3 (_,_,x) = x


{-# DEPRECATED sel4_1, sel4_2, sel4_3, sel4_4 "Use the tuple package instead." #-}
-- 4-tuples
sel4_1 :: (a, b, c, d) -> a
sel4_1 (x,_,_,_) = x
sel4_2 :: (a, b, c, d) -> b
sel4_2 (_,x,_,_) = x
sel4_3 :: (a, b, c, d) -> c
sel4_3 (_,_,x,_) = x
sel4_4 :: (a, b, c, d) -> d
sel4_4 (_,_,_,x) = x


-- 5-tuples

{-# DEPRECATED sel5_1, sel5_2, sel5_3, sel5_4, sel5_5 "Use the tuple package instead." #-}
sel5_1 :: (a, b, c, d, e) -> a
sel5_1 (x,_,_,_,_) = x
sel5_2 :: (a, b, c, d, e) -> b
sel5_2 (_,x,_,_,_) = x
sel5_3 :: (a, b, c, d, e) -> c
sel5_3 (_,_,x,_,_) = x
sel5_4 :: (a, b, c, d, e) -> d
sel5_4 (_,_,_,x,_) = x
sel5_5 :: (a, b, c, d, e) -> e
sel5_5 (_,_,_,_,x) = x


------------------------------------------------------------------------------
-- Floating point utilities
------------------------------------------------------------------------------

-- Floating-point div and modulo operators.

{-# DEPRECATED fDiv, fMod, fDivMod "These are not used by Yampa and will be removed." #-}
fDiv :: (RealFrac a) => a -> a -> Integer
fDiv x y = fst (fDivMod x y)


fMod :: (RealFrac a) => a -> a -> a
fMod x y = snd (fDivMod x y)


fDivMod :: (RealFrac a) => a -> a -> (Integer, a)
fDivMod x y = (q, r)
    where
        q = (floor (x/y))
        r = x - fromIntegral q * y

-- * Arrows
------------------------------------------------------------------------------
-- Liftings
------------------------------------------------------------------------------

arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry


arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d


arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e


arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f


lift0 :: Arrow a => c -> a b c
lift0 c = arr (const c)


lift1 :: Arrow a => (c -> d) -> (a b c -> a b d)
lift1 f = \a -> a >>> arr f


lift2 :: Arrow a => (c -> d -> e) -> (a b c -> a b d -> a b e)
lift2 f = \a1 a2 -> a1 &&& a2 >>> arr2 f


lift3 :: Arrow a => (c -> d -> e -> f) -> (a b c -> a b d -> a b e -> a b f)
lift3 f = \a1 a2 a3 -> (lift2 f) a1 a2 &&& a3 >>> arr2 ($)


lift4 :: Arrow a => (c->d->e->f->g) -> (a b c->a b d->a b e->a b f->a b g)
lift4 f = \a1 a2 a3 a4 -> (lift3 f) a1 a2 a3 &&& a4 >>> arr2 ($)


lift5 :: Arrow a =>
    (c->d->e->f->g->h) -> (a b c->a b d->a b e->a b f->a b g->a b h)
lift5 f = \a1 a2 a3 a4 a5 ->(lift4 f) a1 a2 a3 a4 &&& a5 >>> arr2 ($)
