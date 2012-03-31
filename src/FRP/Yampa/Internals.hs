-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Internals
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- An interface giving access to some of the internal
-- details of the Yampa implementation.
--
-- This interface is indended to be used when the need arises to break
-- abstraction barriers, e.g. for interfacing Yampa to the real world, for
-- debugging purposes, or the like. Be aware that the internal details
-- may change. Relying on this interface means that your code is not
-- insulated against such changes.
-----------------------------------------------------------------------------------------

module FRP.Yampa.Internals (
    Event(..)		-- The event type, its constructors, and instances.
) where

import FRP.Yampa.Event


------------------------------------------------------------------------------
-- Extra Event instances
------------------------------------------------------------------------------

instance Show a => Show (Event a) where
    showsPrec d NoEvent   = showString "NoEvent"
    showsPrec d (Event a) = showParen (d >= 10)
				      (showString "Event " . showsPrec 10 a)


