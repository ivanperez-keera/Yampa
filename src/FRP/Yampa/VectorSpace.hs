{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.VectorSpace
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Vector space type relation and basic instances.
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.VectorSpace where

------------------------------------------------------------------------------
-- Vector space type relation
------------------------------------------------------------------------------

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

-- Maybe norm and normalize should not be class methods, in which case
-- the constraint on the coefficient space (a) should (or, at least, could)
-- be Fractional (roughly a Field) rather than Floating.

-- Minimal instance: zeroVector, (*^), (^+^), dot
class Floating a => VectorSpace v a | v -> a where
    zeroVector   :: v
    (*^)         :: a -> v -> v
    (^/)         :: v -> a -> v
    negateVector :: v -> v
    (^+^)        :: v -> v -> v
    (^-^)        :: v -> v -> v
    dot          :: v -> v -> a
    norm	 :: v -> a
    normalize	 :: v -> v

    v ^/ a = (1/a) *^ v

    negateVector v = (-1) *^ v

    v1 ^-^ v2 = v1 ^+^ negateVector v2

    norm v = sqrt (v `dot` v)

    normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
        where
	    nv = norm v

------------------------------------------------------------------------------
-- Vector space instances for Float and Double
------------------------------------------------------------------------------

instance VectorSpace Float Float where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


instance VectorSpace Double Double where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


------------------------------------------------------------------------------
-- Vector space instances for small tuples of Floating
------------------------------------------------------------------------------

instance Floating a => VectorSpace (a,a) a where
    zeroVector = (0,0)

    a *^ (x,y) = (a * x, a * y)

    (x,y) ^/ a = (x / a, y / a)

    negateVector (x,y) = (-x, -y)

    (x1,y1) ^+^ (x2,y2) = (x1 + x2, y1 + y2)

    (x1,y1) ^-^ (x2,y2) = (x1 - x2, y1 - y2)

    (x1,y1) `dot` (x2,y2) = x1 * x2 + y1 * y2


instance Floating a => VectorSpace (a,a,a) a where
    zeroVector = (0,0,0)

    a *^ (x,y,z) = (a * x, a * y, a * z)

    (x,y,z) ^/ a = (x / a, y / a, z / a)

    negateVector (x,y,z) = (-x, -y, -z)

    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2


instance Floating a => VectorSpace (a,a,a,a) a where
    zeroVector = (0,0,0,0)

    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)

    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)

    negateVector (x,y,z,u) = (-x, -y, -z, -u)

    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)

    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2


instance Floating a => VectorSpace (a,a,a,a,a) a where
    zeroVector = (0,0,0,0,0)

    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)

    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)

    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)

    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)

    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2



