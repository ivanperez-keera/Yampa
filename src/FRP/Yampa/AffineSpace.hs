{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.AffineSpace
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Affine space type relation.
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.AffineSpace where

import FRP.Yampa.VectorSpace

infix 6 .+^, .-^, .-.

-- Maybe origin should not be a class method, even though an origin
-- can be assocoated with any affine space.
-- Maybe distance should not be a class method, in which case the constraint
-- on the coefficient space (a) could be Fractional (i.e., a Field), which
-- seems closer to the mathematical definition of affine space, provided
-- the constraint on the coefficient space for VectorSpace is also Fractional.

-- | Affine Space type relation.
--
-- An affine space is a set (type) @p@, and an associated vector space @v@ over
-- a field @a@.
class (Floating a, VectorSpace v a) => AffineSpace p v a | p -> v, v -> a where

    -- | Origin of the affine space.
    origin   :: p

    -- | Addition of affine point and vector.
    (.+^)    :: p -> v -> p

    -- | Subtraction of affine point and vector.
    (.-^)    :: p -> v -> p
    p .-^ v = p .+^ (negateVector v)

    -- | Subtraction of two points in the affine space, giving a vector.
    (.-.)    :: p -> p -> v

    -- | Distance between two points in the affine space, same as the 'norm' of
    -- the vector they form (see '(.-.)'.
    distance :: p -> p -> a
    distance p1 p2 = norm (p1 .-. p2)
