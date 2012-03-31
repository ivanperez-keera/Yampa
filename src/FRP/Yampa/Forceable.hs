-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Forceable
-- Copyright   :  (c) Zhanyong Wan, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Hyperstrict evaluation.
-----------------------------------------------------------------------------------------

module FRP.Yampa.Forceable where


class Forceable a where
    force :: a -> a


instance Forceable Int where
  force = id


instance Forceable Integer where
  force = id


instance Forceable Double where
  force = id


instance Forceable Float where
  force = id


instance Forceable Bool where
  force = id


instance Forceable () where
  force = id


instance Forceable Char where
  force = id


instance (Forceable a, Forceable b) => Forceable (a, b) where
  force p@(a, b) = force a `seq` force b `seq` p


instance (Forceable a, Forceable b, Forceable c) => Forceable (a, b, c) where
  force p@(a, b, c) = force a `seq` force b `seq` force c `seq` p


instance (Forceable a, Forceable b, Forceable c, Forceable d) =>
         Forceable (a, b, c, d) where
  force p@(a, b, c, d) =
      force a `seq` force b `seq` force c `seq` force d `seq` p


instance (Forceable a, Forceable b, Forceable c, Forceable d, Forceable e) =>
         Forceable (a, b, c, d, e) where
  force p@(a, b, c, d, e) =
      force a `seq` force b `seq` force c `seq` force d `seq` force e `seq` p


instance (Forceable a) => Forceable [a] where
  force nil@[] = nil
  force xs@(x:xs') = force x `seq` force xs' `seq` xs


instance (Forceable a) => Forceable (Maybe a) where
  force mx@Nothing  = mx
  force mx@(Just x) = force x `seq` mx
