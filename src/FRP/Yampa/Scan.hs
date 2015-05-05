{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Scan
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
-----------------------------------------------------------------------------------------

module FRP.Yampa.Scan (
-- ** Simple, stateful signal processing
    sscan,              -- :: (b -> a -> b) -> b -> SF a b
    sscanPrim,          -- :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
) where

import FRP.Yampa.InternalCore (SF(..), sfSScan)

------------------------------------------------------------------------------
-- Simple, stateful signal processing
------------------------------------------------------------------------------

-- New sscan primitive. It should be possible to define lots of functions
-- in terms of this one. Eventually a new constructor will be introduced if
-- this works out.

sscan :: (b -> a -> b) -> b -> SF a b
sscan f b_init = sscanPrim f' b_init b_init
    where
        f' b a = let b' = f b a in Just (b', b')

sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f c_init b_init = SF {sfTF = tf0}
    where
        tf0 a0 = case f c_init a0 of
                     Nothing       -> (sfSScan f c_init b_init, b_init)
                     Just (c', b') -> (sfSScan f c' b', b')

-- Vim modeline
-- vim:set tabstop=8 expandtab:
