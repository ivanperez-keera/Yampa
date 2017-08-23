-----------------------------------------------------------------------------------------
-- |
-- Module      :  FRP.Yampa.Miscellany
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Framework for record merging.
--
-- Idea:
--
-- MergeableRecord is intended to be a super class for classes providing
-- update operations on records. The ADT induced by such a set of operations
-- can be considered a "mergeable record", which can be merged into larger
-- mergeable records essentially by function composition. Finalization turns
-- a mergeable record into a record.
--
-- Typical use:
--
-- Given
--
-- >  data Foo = Foo {l1 :: T1, l2 :: T2}
--
-- one define a mergeable record type (MR Foo) by the following instance:
--
-- @
--   instance MergeableRecord Foo where
--       mrDefault = Foo {l1 = v1_dflt, l2 = v2_dflt}
-- @
--
-- Typically, one would also provide definitions for setting the fields,
-- possibly (but not necessarily) overloaded:
--
-- @
--   instance HasL1 Foo where
--       setL1 v = mrMake (\foo -> foo {l1 = v})
-- @
--
-- Now Foo records can be created as follows:
--
-- @
--   let foo1 = setL1 v1
--   ...
--   let foo2 = setL2 v2 ~+~ foo1
--   ...
--   let foo<N> = setL1 vN ~+~ foo<N-1>
--   let fooFinal = mrFinalize foo<N>
-- @
-----------------------------------------------------------------------------------------

module FRP.Yampa.MergeableRecord (
    MergeableRecord(..),
    MR,                 -- Abstract
    mrMake,
    (~+~),
    mrMerge,
    mrFinalize
) where

class MergeableRecord a where
    mrDefault :: a


-- Type constructor for mergeable records.
newtype MR a = MR (a -> a)


-- Construction of a mergeable record.
mrMake :: MergeableRecord a => (a -> a) -> MR a
mrMake f = (MR f)


-- Merge two mergeable records. Left "overrides" in case of conflict.
(~+~) :: MergeableRecord a => MR a -> MR a -> MR a
(MR f1) ~+~ (MR f2) = MR (f1 . f2)

mrMerge :: MergeableRecord a => MR a -> MR a -> MR a
mrMerge = (~+~)


-- Finalization: turn a mergeable record into a record.
mrFinalize :: MergeableRecord a => MR a -> a
mrFinalize (MR f) = f mrDefault
