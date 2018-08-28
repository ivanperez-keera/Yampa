{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Vector2
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 2D vector abstraction (R^2).
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Vector2 (
    Vector2,            -- Abstract, instance of VectorSpace
    vector2,            -- :: RealFloat a => a -> a -> Vector2 a
    vector2X,           -- :: RealFloat a => Vector2 a -> a
    vector2Y,           -- :: RealFloat a => Vector2 a -> a
    vector2XY,          -- :: RealFloat a => Vector2 a -> (a, a)
    vector2Polar,       -- :: RealFloat a => a -> a -> Vector2 a
    vector2Rho,         -- :: RealFloat a => Vector2 a -> a
    vector2Theta,       -- :: RealFloat a => Vector2 a -> a
    vector2RhoTheta,    -- :: RealFloat a => Vector2 a -> (a, a)
    vector2Rotate       -- :: RealFloat a => a -> Vector2 a -> Vector2 a
) where

import FRP.Yampa.VectorSpace
import FRP.Yampa.Forceable

-- * 2D vector, constructors and selectors

-- | 2D Vector.

-- Restrict coefficient space to RealFloat (rather than Floating) for now.
-- While unclear if a complex coefficient space would be useful (and if the
-- result really would be a 2d vector), the only thing causing trouble is the
-- use of atan2 in vector2Theta. Maybe atan2 can be generalized?

data Vector2 a = RealFloat a => Vector2 !a !a

deriving instance Eq a => Eq (Vector2 a)

deriving instance Show a => Show (Vector2 a)

-- | Creates a 2D vector from the cartesian coordinates.
vector2 :: RealFloat a => a -> a -> Vector2 a
vector2 = Vector2

-- | X cartesian coordinate.
vector2X :: RealFloat a => Vector2 a -> a
vector2X (Vector2 x _) = x

-- | Y cartesian coordinate.
vector2Y :: RealFloat a => Vector2 a -> a
vector2Y (Vector2 _ y) = y

-- | Returns a vector's cartesian coordinates.
vector2XY :: RealFloat a => Vector2 a -> (a, a)
vector2XY (Vector2 x y) = (x, y)

-- | Creates a 2D vector from the polar coordinates.
vector2Polar :: RealFloat a => a -> a -> Vector2 a
vector2Polar rho theta = Vector2 (rho * cos theta) (rho * sin theta)

-- | Calculates the vector's radial distance (magnitude).
vector2Rho :: RealFloat a => Vector2 a -> a
vector2Rho (Vector2 x y) = sqrt (x * x + y * y)

-- | Calculates the vector's azimuth (angle).
vector2Theta :: RealFloat a => Vector2 a -> a
vector2Theta (Vector2 x y) = atan2 y x

-- | Polar coordinate representation of a 2D vector.
vector2RhoTheta :: RealFloat a => Vector2 a -> (a, a)
vector2RhoTheta v = (vector2Rho v, vector2Theta v)

-- * Vector space instance

instance RealFloat a => VectorSpace (Vector2 a) a where
    zeroVector = Vector2 0 0

    a *^ (Vector2 x y) = Vector2 (a * x) (a * y)

    (Vector2 x y) ^/ a = Vector2 (x / a) (y / a)

    negateVector (Vector2 x y) = (Vector2 (-x) (-y))

    (Vector2 x1 y1) ^+^ (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

    (Vector2 x1 y1) ^-^ (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

    (Vector2 x1 y1) `dot` (Vector2 x2 y2) = x1 * x2 + y1 * y2


-- * Additional operations

-- | Rotates a vector with a given angle.
vector2Rotate :: RealFloat a => a -> Vector2 a -> Vector2 a
vector2Rotate theta' v = vector2Polar (vector2Rho v) (vector2Theta v + theta')


-- * Forceable instance

instance RealFloat a => Forceable (Vector2 a) where
     force = id
