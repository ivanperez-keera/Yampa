{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Point2
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 2D point abstraction (R^2).
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Point2 (
    -- module AFRPVectorSpace,
    -- module AFRPAffineSpace,
    -- module AFRPVector2,
    Point2(..), -- Non-abstract, instance of AffineSpace
    point2X,    -- :: RealFloat a => Point2 a -> a
    point2Y     -- :: RealFloat a => Point2 a -> a
) where

import FRP.Yampa.VectorSpace ()
import FRP.Yampa.AffineSpace
import FRP.Yampa.Vector2
import FRP.Yampa.Forceable

------------------------------------------------------------------------------
-- 2D point, constructors and selectors.
------------------------------------------------------------------------------

data Point2 a = RealFloat a => Point2 !a !a

deriving instance Eq a => Eq (Point2 a)

deriving instance Show a => Show (Point2 a)

point2X :: RealFloat a => Point2 a -> a
point2X (Point2 x _) = x

point2Y :: RealFloat a => Point2 a -> a
point2Y (Point2 _ y) = y


------------------------------------------------------------------------------
-- Affine space instance
------------------------------------------------------------------------------

instance RealFloat a => AffineSpace (Point2 a) (Vector2 a) a where
    origin = Point2 0 0

    (Point2 x y) .+^ v = Point2 (x + vector2X v) (y + vector2Y v)

    (Point2 x y) .-^ v = Point2 (x - vector2X v) (y - vector2Y v)

    (Point2 x1 y1) .-. (Point2 x2 y2) = vector2 (x1 - x2) (y1 - y2)


------------------------------------------------------------------------------
-- Forceable instance
------------------------------------------------------------------------------

instance RealFloat a => Forceable (Point2 a) where
     force = id
