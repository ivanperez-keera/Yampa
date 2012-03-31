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

------------------------------------------------------------------------------
-- Affine Space type relation
------------------------------------------------------------------------------

infix 6 .+^, .-^, .-.

-- Maybe origin should not be a class method, even though an origin
-- can be assocoated with any affine space.
-- Maybe distance should not be a class method, in which case the constraint
-- on the coefficient space (a) could be Fractional (i.e., a Field), which
-- seems closer to the mathematical definition of affine space, provided
-- the constraint on the coefficient space for VectorSpace is also Fractional.

-- Minimal instance: origin, .+^, .^.
class (Floating a, VectorSpace v a) => AffineSpace p v a | p -> v, v -> a where
    origin   :: p
    (.+^)    :: p -> v -> p
    (.-^)    :: p -> v -> p
    (.-.)    :: p -> p -> v
    distance :: p -> p -> a

    p .-^ v = p .+^ (negateVector v)

    distance p1 p2 = norm (p1 .-. p2)
