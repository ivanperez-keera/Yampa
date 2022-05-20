{-# LANGUAGE GADTs  #-}
{-# LANGUAGE Arrows #-}
-- TODO
-- Properties in this file have different types.
-- It's important to agree on the representation type.
--
-- It may be a bit hard, because some elements from logic are
-- provided by QC, while others have to be defined by us.
-- For example, connectives like implication and always are
-- provided by us, and forAll is in QuickCheck.
--
-- This makes it hard to combine, becase for this language to be
-- compositional like logic is we need to make everything accept
-- a QuickCheck predicate, which may not be possible or compatible
-- with out goals.
--
-- Notes pertaining to regression tests:
-- - Add test cases for Yampa. There should be at least one test case for each
--   "non-trivial" entity exported from Yampa.
--
-- - Make tests cases for after and repeatedly more robust.  Must not
--   fail due to small discrepancies in floating point implementation.
--
--   01-May-2002:  evsrc_t7 currently fails in hugs.
--
-- - Restructure test cases for papallel composition and switches to reflect
--   Yampa structure better. Separate test cases for the generic definitions?
-- There are some test cases for Utils. Not intended to be exhaustive.
--
-- VectorSpace has caused some ambiguity problems. See e.g. looplaws_t2,
-- switch_t1a.
--
-- 2005-11-26: A simple way of making many test cases more robust would
-- be to have a version of deltaEncode that adds a little extra time
-- to the very first delta time. That way sampling would always be slightly
-- "late".
--
-- But since we often compare time stamps, we'd also either have
-- to adjust the "~=" relation to tolerate "jitter" of that magnitute,
-- or we'd have to formulate many tests more carefully to allow a
-- certain "fuzziness".
module Main where

import Data.Fixed

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.EventS (snap)
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

-- Local tests
import qualified TestsAccum        as Regression
import qualified TestsArr          as Regression
import qualified TestsComp         as Regression
import qualified TestsEmbed        as Regression
import qualified TestsEvSrc        as Regression
import qualified TestsFirstSecond  as Regression
import qualified TestsLaws         as Regression
import qualified TestsLoop         as Regression
import qualified TestsLoopIntegral as Regression
import qualified TestsLoopLaws     as Regression
import qualified TestsLoopPre      as Regression
import qualified TestsRSwitch      as Regression
import qualified TestsReact        as Regression
import qualified TestsSscan        as Regression
import qualified TestsUtils        as Regression
import qualified TestsWFG          as Regression

import qualified Test.FRP.Yampa.Basic       as NewBasic
import qualified Test.FRP.Yampa.Conditional as NewConditional
import qualified Test.FRP.Yampa.Delays      as NewDelays
import qualified Test.FRP.Yampa.Integration as NewIntegration
import qualified Test.FRP.Yampa.Switches    as NewSwitches
import qualified Test.FRP.Yampa.Task        as NewTask
import qualified Test.FRP.Yampa.Time        as NewTime

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Yampa QC properties"
  [ testProperty "Identity"                               prop_arr_id
  , testProperty "Arrow Naturality"                       prop_arr_naturality
  , testProperty "Naturality"                             prop_arr_naturality
  , testProperty "Arrows > Composition (1)"               prop_arrow_comp_1
  , testProperty "Arrows > Composition (2)"               prop_arrow_comp_2
  , testProperty "Arrows > Composition (3)"               prop_arrow_comp_3
  -- FIXME: (iperez:) delay_t3 is not here because I can't understand it.
  -- Missing: delay t4 and t5
  -- Missing: embed
  , testProperty "Events > No event"                      prop_event_noevent
  , testProperty "Events > Now"                           prop_event_now
  , testProperty "Events > After 0.0"                     prop_event_after_0
  -- Missing: a lot of event tests
  , testProperty "Arrows > First (1)"                     prop_arrow_first_1
  , testProperty "Arrows > First (2)"                     prop_arrow_first_2
  , testProperty "Arrows > Second (1)"                    prop_arrow_second_1
  , testProperty "Arrows > Second (2)"                    prop_arrow_second_2
  -- Missing: first and second with integrals
  -- Missing: KSwitch

  , testProperty "Arrows > Identity (0)"                  prop_arrow_id_0
  , testProperty "Arrows > Identity (2)"                  prop_arrow_id_2
  , testProperty "Arrows > Associativity"                 prop_arrow_assoc
  , testProperty "Arrows > Function lifting composition"  prop_arrow_arr_comp
  , testProperty "Arrows > First"                         prop_arrow_first_3
  , testProperty "Arrows > Distributivity of First"       prop_arrow_first_distrib
  , testProperty "Arrows > Commutativity of id on first"  prop_arrow_first_id_comm
  , testProperty "Arrows > Nested firsts"                 prop_arrow_first_nested

  -- Missing: Loop *
  -- Missing: PSwitch
  -- Missing: iPre
  -- Missing: RPSwitch
  -- Missing: RSwitch
  -- Missing: React
  -- Missing: Sscan
  -- Missing: Switch
  -- , testProperty "Switching > t1"                 prop_switch_t1
  -- Missing: Task
  -- Missing: Utils
  -- Missing: WFG

  , testProperty "Regression > arr"           (property $ and Regression.arr_trs)
  , testProperty "Regression > comp"          (property $ and Regression.comp_trs)
  , testProperty "Regression > first"         (property $ and Regression.first_trs)
  , testProperty "Regression > second"        (property $ and Regression.second_trs)
  , testProperty "Regression > laws"          (property $ and Regression.laws_trs)
  , testProperty "Regression > loop"          (property $ and Regression.loop_trs)
  , testProperty "Regression > looplaws"      (property $ and Regression.looplaws_trs)
  , testProperty "Regression > sscan"         (property $ and Regression.sscan_trs)
  , testProperty "Regression > evsrc"         (property $ and Regression.evsrc_trs)
  , testProperty "Regression > rswitch"       (property $ and Regression.rswitch_trs)
  , testProperty "Regression > wfg"           (property $ and Regression.wfg_trs)
  , testProperty "Regression > accum"         (property $ and Regression.accum_trs)
  , testProperty "Regression > loopPre"       (property $ and Regression.loopPre_trs)
  , testProperty "Regression > loopIntegral"  (property $ and Regression.loopIntegral_trs)
  , testProperty "Regression > react"         (property $ and Regression.react_trs)
  , testProperty "Regression > embed"         (property $ and Regression.embed_trs)
  , testProperty "Regression > utils"         (property $ and Regression.utils_trs)
  , NewBasic.tests
  , NewConditional.tests
  , NewDelays.tests
  , NewIntegration.tests
  , NewHybrid.tests
  , NewSimulation.tests
  , NewTask.tests
  , NewSwitches.tests
  , NewTime.tests
  ]

-- * Yampa laws

-- ** Arrow laws

prop_arr_id =
    forAll myStream (evalT $ prop_always_equal (arr id) identity)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

-- Yampa's internal test cases

-- prop :: SF a b -> (a -> b ->
prop (a,b) = SP ((identity &&& a) >>^ uncurry b)

-- Yampa's Arrow Checks

-- C1: Arr naturality (testSF1 (arr (+1)))
-- C2: Arr naturality (testSF2 (arr (+1)))
prop_arr_naturality =
    forAll myStream $ \stream ->
      forAll f $ \f' ->
        evalT (Always (prop (arr (apply f'), \x y -> apply f' x == y)))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        f :: Gen (Fun Int Int)
        f = arbitrary

prop_arrow_1 = forAll myStream $ evalT $
    Always $ prop (arr id, \x y -> x == y)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

prop_arrow_2 = forAll myStream $ evalT $
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        sf1 = arr (f >>> g)
        sf2 = arr f >>> arr g
        f = (+5)
        g = (/20)

prop_arrow_2' =
    forAll f $ \f' ->
      forAll g $ \g' ->
        forAll myStream $ evalT $
          prop_arrow_2'' (apply f') (apply g')

  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        f, g :: Gen (Fun Int Int)
        f = arbitrary
        g = arbitrary

prop_arrow_2'' f g =
    Always $ prop (sf1 &&& sf2, const $ uncurry (==))
  where sf1 = arr (f >>> g)
        sf2 = arr f >>> arr g

-- Arrow composition (we use Int to avoid floating-point discrepancies)
prop_arrow_comp_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr (+1) >>> arr (+2)
        pred = (\x y -> x + 3 == y)

-- Arrow composition
prop_arrow_comp_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        sf   = constant 5.0 >>> arr (+1)
        pred = const (== 6.0)

-- Arrow composition
prop_arrow_comp_3 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Float)
        myStream = fixedDelayStream 0.25

        sf :: SF a Float
        sf = constant 2.0 >>> integral >>> stepDiff (-0.5)

        pred = const (== 0.5)

prop_insert =
    forAll initialValueG $ \initialValue ->
    forAll finalValueG $ \finalValue ->
    forAll myStream $ evalT $
      let sfStep = initialValue --> constant finalValue

      in And (prop (sfStep, const (== initialValue)))
             (Next $ Always $
                       (prop (sfStep, const (== finalValue))))

  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

        initialValueG :: Gen Float
        initialValueG = arbitrary

        finalValueG  :: Gen Float
        finalValueG = arbitrary

stepDiff :: Num a => a -> SF a a
stepDiff z = loopPre z (arr (\(x,y) -> (x - y, x)))

-- Events
prop_event_noevent =
    forAll myStream $ evalT $ Always $ prop (sfNever, const (== noEvent))

  where myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream
        sfNever :: SF Float (Event Float)
        sfNever = never

prop_event_now =
    forAll myStream $ evalT $
      -- (sf, p0) /\ O [] (sf, pn)
      And (prop (sf, p0))                 -- Initially
          (Next $ Always $ prop (sf, pn)) -- After first sample

  where sf = Yampa.now 42.0

        p0 x y = y == Event 42.0
        pn x y = y == noEvent

        myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

prop_event_after_0 =
    forAll myStream $ evalT $
      -- (sf, p0) /\ O [] (sf, pn)
      And (prop (sf, p0))                 -- Initially
          (Next $ Always $ prop (sf, pn)) -- After first sample

  where sf = after 0.0 42.0

        p0 x y = y == Event 42.0
        pn x y = y == noEvent

        myStream :: Gen (SignalSampleStream Float)
        myStream = uniDistStream

prop_arrow_first_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> first (constant 7)
        pred = (\x y -> (7 :: Int, x) == y)

prop_arrow_first_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> first (arr (+1))
        pred = (\x y -> (x + 1, x) == y)

prop_arrow_second_1 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> second (constant 7)
        pred = (\x y -> (x, 7 :: Int) == y)

prop_arrow_second_2 =
    forAll myStream $ evalT $ Always $ prop (sf, pred)
  where myStream :: Gen (SignalSampleStream Int)
        myStream = uniDistStream

        sf   = arr dup >>> second (arr (+1))
        pred = (\x y -> (x, x + 1) == y)

prop_arrow_id_0 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = arr id >>> integral
        sf2 = integral
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_id_2 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = integral >>> arr id
        sf2 = integral
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_assoc =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = (integral >>> arr (*0.5)) >>> integral
        sf2 = integral >>> (arr (*0.5) >>> integral)
        pred = arr $ uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_arr_comp =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> pred)
  where sf1 = (arr ((*2.5) . (+3.0)))
        sf2 = (arr (+3.0) >>> arr (*2.5))
        pred = arr (uncurry (==))

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_3 =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> first (arr (*2.5)))
        sf2 = (arr dup >>> arr (fun_prod (*2.5) id))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_distrib =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> (first (integral >>> arr (+3.0))))
        sf2 = (arr dup >>> (first integral >>> first (arr (+3.0))))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_id_comm =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where sf1 = (arr dup >>> (first integral>>>arr (fun_prod id (+3.0))))
        sf2 = (arr dup >>> (arr (fun_prod id (+3.0))>>>first integral))
        pred = uncurry (==)

        myStream :: Gen (SignalSampleStream Double)
        myStream = uniDistStream

prop_arrow_first_nested =
    forAll myStream $ evalT $ Always $ SP ((sf1 &&& sf2) >>> arr pred)
  where
    sf1 = (arr (\x -> ((x,x),())) >>> (first (first integral) >>> arr assoc))
    sf2 = (arr (\x -> ((x,x),())) >>> (arr assoc >>> first integral))

    pred = uncurry (==)

    myStream :: Gen (SignalSampleStream Double)
    myStream = uniDistStream

delayedF = arr id &&& cond
  where cond = after 1.5 (Event ())

-- * Generic SF predicate building functions

-- | Compares two SFs, resulting in true if they are always equal
prop_always_equal sf1 sf2 =
    Always $ SP ((sf1 &&& sf2) >>> arr sameResult)
  where sameResult = uncurry (==)

prop_arr_no_change f xs =
     samples (fst (evalSF (arr f) xs)) == map f (samples xs)

-- | Compares two SFs, returning true if they are close enough
prop_always_similar margin sf1 sf2 =
    Always (SP ((sf1 &&& sf2) >>> arr similar))
  where similar (x,y) = abs (x-y) <= margin

sfMeasureIncrement :: Num b => b -> SF a b -> SF a b
sfMeasureIncrement init sf = loopPre init sf'
  where sf' = (sf *** identity) >>> arr (\(n, o) -> (n - o, n))

fun_prod f g = \(x,y) -> (f x, g y)

assoc :: ((a,b),c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))

assocInv :: (a,(b,c)) -> ((a,b),c)
assocInv (a,(b,c)) = ((a,b),c)
