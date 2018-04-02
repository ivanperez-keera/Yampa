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
-----------------------------------------------------------------------------------------

module FRP.Yampa.VectorSpace where

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

-- Maybe norm and normalize should not be class methods, in which case
-- the constraint on the coefficient space (a) should (or, at least, could)
-- be Fractional (roughly a Field) rather than Floating.

-- | Vector space type relation.
--
--   A vector space is a set (type) closed under addition and multiplication by
--   a scalar. The type of the scalar is the /field/ of the vector space, and
--   it is said that @v@ is a vector space over @a@.
--
--   The encoding uses a type class |VectorSpace| @v a@, where @v@ represents
--   the type of the vectors and @a@ represents the types of the scalars.

class (Eq a, Floating a) => VectorSpace v a | v -> a where
    -- | Vector with no magnitude (unit for addition).
    zeroVector :: v

    -- | Multiplication by a scalar.
    (*^) :: a -> v -> v

    -- | Division by a scalar.
    (^/) :: v -> a -> v
    v ^/ a = (1/a) *^ v

    -- | Vector addition
    (^+^) :: v -> v -> v

    -- | Vector subtraction
    (^-^) :: v -> v -> v
    v1 ^-^ v2 = v1 ^+^ negateVector v2

    -- | Vector negation. Addition with a negated vector should be
    --   same as subtraction.
    negateVector :: v -> v
    negateVector v = (-1) *^ v

    -- | Dot product (also known as scalar or inner product).
    --
    -- For two vectors, mathematically represented as @a = a1,a2,...,an@ and @b
    -- = b1,b2,...,bn@, the dot product is @a . b = a1*b1 + a2*b2 + ... +
    -- an*bn@.
    --
    -- Some properties are derived from this. The dot product of a vector with
    -- itself is the square of its magnitude ('norm'), and the dot product of
    -- two orthogonal vectors is zero.
    dot :: v -> v -> a

    -- | Vector's norm (also known as magnitude).
    --
    -- For a vector represented mathematically as @a = a1,a2,...,an@, the norm
    -- is the square root of @a1^2 + a2^2 + ... + an^2@.
    norm :: v -> a
    norm v = sqrt (v `dot` v)

    -- | Return a vector with the same origin and orientation (angle), but such
    -- that the norm is one (the unit for multiplication by a scalar).
    normalize    :: v -> v
    normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
        where nv = norm v

-- | Vector space instance for 'Float's, with 'Float' scalars.
instance VectorSpace Float Float where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2

-- | Vector space instance for 'Double's, with 'Double' scalars.
instance VectorSpace Double Double where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


-- | Vector space instance for pairs of 'Floating' point numbers.
instance (Eq a, Floating a) => VectorSpace (a,a) a where
    zeroVector = (0,0)

    a *^ (x,y) = (a * x, a * y)

    (x,y) ^/ a = (x / a, y / a)

    negateVector (x,y) = (-x, -y)

    (x1,y1) ^+^ (x2,y2) = (x1 + x2, y1 + y2)

    (x1,y1) ^-^ (x2,y2) = (x1 - x2, y1 - y2)

    (x1,y1) `dot` (x2,y2) = x1 * x2 + y1 * y2

-- | Vector space instance for triplets of 'Floating' point numbers.
instance (Eq a, Floating a) => VectorSpace (a,a,a) a where
    zeroVector = (0,0,0)

    a *^ (x,y,z) = (a * x, a * y, a * z)

    (x,y,z) ^/ a = (x / a, y / a, z / a)

    negateVector (x,y,z) = (-x, -y, -z)

    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Vector space instance for tuples with four 'Floating' point numbers.
instance (Eq a, Floating a) => VectorSpace (a,a,a,a) a where
    zeroVector = (0,0,0,0)

    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)

    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)

    negateVector (x,y,z,u) = (-x, -y, -z, -u)

    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)

    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2

-- | Vector space instance for tuples with five 'Floating' point numbers.
instance (Eq a, Floating a) => VectorSpace (a,a,a,a,a) a where
    zeroVector = (0,0,0,0,0)

    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)

    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)

    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)

    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)

    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2
