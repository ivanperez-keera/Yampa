-- |
-- Description : Test cases for FRP.Yampa.Switches
-- Copyright   : Yale University, 2003
-- Authors     : Antony Courtney and Henrik Nilsson
module Test.FRP.Yampa.Switches
    ( tests
    )
  where

import Data.Fixed

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.EventS (snap)
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Switches"
  [ testProperty "parB (fixed)" (property $ coc_t0 ~= coc_t0r)
  , testProperty "parB (qc)"    prop_broadcast
  ]

-- * Test cases for collection-oriented combinators

coc_inp1 = deltaEncode 0.1 [0.0, 0.5 ..]

coc_t0 :: [[Double]]
coc_t0 = take 20 $ embed (parB [constant 1.0, identity, integral]) coc_inp1

coc_t0r =
  [ [1.0, 0.0, 0.00]
  , [1.0, 0.5, 0.00]
  , [1.0, 1.0, 0.05]
  , [1.0, 1.5, 0.15]
  , [1.0, 2.0, 0.30]
  , [1.0, 2.5, 0.50]
  , [1.0, 3.0, 0.75]
  , [1.0, 3.5, 1.05]
  , [1.0, 4.0, 1.40]
  , [1.0, 4.5, 1.80]
  , [1.0, 5.0, 2.25]
  , [1.0, 5.5, 2.75]
  , [1.0, 6.0, 3.30]
  , [1.0, 6.5, 3.90]
  , [1.0, 7.0, 4.55]
  , [1.0, 7.5, 5.25]
  , [1.0, 8.0, 6.00]
  , [1.0, 8.5, 6.80]
  , [1.0, 9.0, 7.65]
  , [1.0, 9.5, 8.55]
  ]

-- Par with broadcast (collection-oriented combinators)
-- TODO: Add integral to the list of SFs being tested
prop_broadcast =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = parB [identity, (arr (+1))]
        pred = (\x [y,z] -> x == y && (x + 1) == z)


-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)
