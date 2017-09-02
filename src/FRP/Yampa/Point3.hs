{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, StandaloneDeriving #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Point3
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 3D point abstraction (R^3).
--
-----------------------------------------------------------------------------------------

module FRP.Yampa.Point3 (
    -- module AFRPVectorSpace,
    -- module AFRPAffineSpace,
    -- module AFRPVector3,
    Point3(..), -- Non-abstract, instance of AffineSpace
    point3X,    -- :: RealFloat a => Point3 a -> a
    point3Y,    -- :: RealFloat a => Point3 a -> a
    point3Z     -- :: RealFloat a => Point3 a -> a
) where

import FRP.Yampa.VectorSpace ()
import FRP.Yampa.AffineSpace
import FRP.Yampa.Vector3
import FRP.Yampa.Forceable

------------------------------------------------------------------------------
-- 3D point, constructors and selectors.
------------------------------------------------------------------------------

data Point3 a = RealFloat a => Point3 !a !a !a

deriving instance Eq a => Eq (Point3 a)

deriving instance Show a => Show (Point3 a)

point3X :: RealFloat a => Point3 a -> a
point3X (Point3 x _ _) = x

point3Y :: RealFloat a => Point3 a -> a
point3Y (Point3 _ y _) = y

point3Z :: RealFloat a => Point3 a -> a
point3Z (Point3 _ _ z) = z


------------------------------------------------------------------------------
-- Affine space instance
------------------------------------------------------------------------------

instance RealFloat a => AffineSpace (Point3 a) (Vector3 a) a where
    origin = Point3 0 0 0

    (Point3 x y z) .+^ v =
        Point3 (x + vector3X v) (y + vector3Y v) (z + vector3Z v)

    (Point3 x y z) .-^ v =
        Point3 (x - vector3X v) (y - vector3Y v) (z - vector3Z v)

    (Point3 x1 y1 z1) .-. (Point3 x2 y2 z2) =
        vector3 (x1 - x2) (y1 - y2) (z1 - z2)


------------------------------------------------------------------------------
-- Forceable instance
------------------------------------------------------------------------------

instance RealFloat a => Forceable (Point3 a) where
     force = id
