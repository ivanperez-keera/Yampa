-- |
-- Description : Test cases for events
-- Copyright   : Ivan Perez, 2022
-- Authors     : Ivan Perez
module Test.FRP.Yampa.Event
    ( tests
    )
  where

import Control.Applicative ((<|>))
import Control.Monad       (guard, join)

import Test.QuickCheck       hiding (once, sample)
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa       (Event (..))
import FRP.Yampa.Event (attach, catEvents, event, filterE, fromEvent, gate,
                        isEvent, isNoEvent, joinE, lMerge, mapFilterE, mapMerge,
                        maybeToEvent, merge, mergeBy, mergeEvents, noEvent,
                        noEventFst, noEventSnd, rMerge, splitE, tag, tagWith)

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Event"
  [ testProperty "noEvent" testNoEvent
  , testProperty "noEvent"      testNoEvent
  , testProperty "noEventFst"   testNoEventFst
  , testProperty "noEventSnd"   testNoEventSnd
  , testEq
  , testOrd
  , testFunctor
  , testApplicative
  , testMonad
  , testAlternative
  , testProperty "maybeToEvent" testMaybeToEvent
  , testEvent
  , testProperty "fromEvent"    testFromEvent
  , testProperty "isEvent"      testIsEvent
  , testProperty "isNoEvent"    testIsNoEvent
  , testProperty "tag"          testTag
  , testProperty "tagWith"      testTagWith
  , testProperty "attach"       testAttach
  , testProperty "lMerge"       testLMerge
  , testProperty "rMerge"       testRMerge
  , testProperty "merge"        testMerge
  , testMergeBy
  , testMapMerge
  , testProperty "mergeEvents"  testMergeEvents
  , testProperty "catEvents"    testCatEvents
  , testProperty "joinE"        testJoinE
  , testProperty "splitE"       testSplitE
  , testProperty "filterE"      testFilterE
  , testProperty "mapFilterE"   testMapFilterE
  , testProperty "gate"         testGate
  ]

-- * The Event type

-- | noEvent
testNoEvent :: Property
testNoEvent = property $
    isNoEvent ev && not (isEvent ev)
  where
    ev :: Event Integer
    ev = noEvent

-- | noEventFst
testNoEventFst :: Property
testNoEventFst =
  forAll randomEvent $ \x ->
    forAll randomEvent $ \y ->
      (isNoEvent $ fst $ noEventFst (x, y))
        && (snd (x, y) == snd (noEventFst (x, y)))

-- | noEventSnd
testNoEventSnd :: Property
testNoEventSnd =
  forAll randomEvent $ \x ->
    forAll randomEvent $ \y ->
      (isNoEvent $ snd $ noEventSnd (x, y))
        && (fst (x, y) == fst (noEventSnd (x, y)))

-- | Eq instance
testEq :: TestTree
testEq = testGroup "equality"
           [ testProperty "equality > reflexive"  testEqReflexive
           , testProperty "equality > symmetric"  testEqSymmetric
           , testProperty "equality > transitive" testEqTransitive
           ]
  where
    testEqReflexive :: Property
    testEqReflexive = forAll randomEvent $ \x -> x == x

    testEqSymmetric :: Property
    testEqSymmetric =
      forAll randomEvent $ \x ->
        forAll randomEvent $ \y ->
          (x == y) <-> (y == x)

    testEqTransitive :: Property
    testEqTransitive =
      forAll randomEvent $ \x ->
        forAll randomEvent $ \y ->
          forAll randomEvent $ \z ->
            ((x == y) && (y == z)) --> (x == z)

-- | Ord instance
testOrd :: TestTree
testOrd = testGroup "ord"
            [ testProperty "ord > reflexive"     testOrdReflexive
            , testProperty "ord > antisymmetric" testOrdAntisymmetric
            , testProperty "ord > transitive"    testOrdTransitive
            ]
  where
    testOrdReflexive :: Property
    testOrdReflexive = forAll randomEvent $ \x -> x <= x

    testOrdAntisymmetric :: Property
    testOrdAntisymmetric =
      forAll randomEvent $ \x ->
        forAll randomEvent $ \y ->
          ((x <= y) && (y <= x)) --> (x == y)

    testOrdTransitive :: Property
    testOrdTransitive =
      forAll randomEvent $ \x ->
        forAll randomEvent $ \y ->
          forAll randomEvent $ \z ->
            ((x == y) && (y == z)) --> (x == z)

-- | Functor instance
testFunctor :: TestTree
testFunctor = testGroup "functor"
    [ testProperty "functor > id"          testFunctorId
    , testProperty "functor > composition" testFunctorComp
    ]
  where
    testFunctorId :: Property
    testFunctorId = forAll randomEvent $ \x ->
      x == fmap id x

    testFunctorComp :: Property
    testFunctorComp =
      forAll randomEvent $ \x ->
        forAllBlind randomFunction $ \f ->
          forAllBlind randomFunction $ \g ->
            fmap f (fmap g x) == fmap (f . g) x

-- | Applicative instance
testApplicative :: TestTree
testApplicative = testGroup "applicative"
    [ testProperty "applicative > id"           testApplicativeId
    , testProperty "applicative > homomorphism" testApplicativeHomomorphism
    , testProperty "applicative > interchange"  testApplicativeInterchange
    , testProperty "applicative > composition"  testApplicativeComposition
    ]
  where
    testApplicativeId :: Property
    testApplicativeId = forAll randomEvent $ \x ->
      x == (pure id <*> x)

    testApplicativeHomomorphism :: Property
    testApplicativeHomomorphism =
      forAllBlind randomFunction $ \f ->
        forAll randomValue $ \x ->
          (pure f <*> pure x) == (pure (f x) :: Event Integer)

    testApplicativeInterchange :: Property
    testApplicativeInterchange =
      forAllBlind randomEventFunctionIn $ \u ->
        forAll randomValue $ \y ->
          (u <*> pure y) == (pure ($ y) <*> u)

    testApplicativeComposition :: Property
    testApplicativeComposition =
      forAllBlind randomEventFunctionIn $ \u ->
        forAllBlind randomEventFunctionIn $ \v ->
          forAll randomEvent $ \w ->
            (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))

-- | Monad instance
testMonad :: TestTree
testMonad = testGroup "monad"
    [ testProperty "monad > id left"    testMonadReturnIdLeft
    , testProperty "monad > id right"   testMonadReturnIdRight
    , testProperty "monad > bind assoc" testMonadBindAssoc
    ]
  where
    testMonadReturnIdLeft :: Property
    testMonadReturnIdLeft =
      forAll randomValue $ \x ->
        forAllBlind randomEventAction $ \f ->
          (return x >>= f) == f x

    testMonadReturnIdRight :: Property
    testMonadReturnIdRight =
      forAll randomValue $ \x ->
        forAll randomEvent $ \f ->
          (f >>= \x -> return x) == f

    testMonadBindAssoc :: Property
    testMonadBindAssoc =
      forAll randomEvent $ \f ->
        forAllBlind randomEventAction $ \g ->
          forAllBlind randomEventAction $ \h ->
            ((f >>= g) >>= h) == (f >>= (\x -> g x >>= h))

-- | Alternative instance
testAlternative :: TestTree
testAlternative = testGroup "alternative"
    [ testProperty "alternative > left id"  testAlternativeEmptyIdLeft
    , testProperty "alternative > right id" testAlternativeEmptyIdRight
    ]
  where
    testAlternativeEmptyIdLeft :: Property
    testAlternativeEmptyIdLeft =
      forAll randomEvent $ \e ->
        (noEvent <|> e) == e

    testAlternativeEmptyIdRight :: Property
    testAlternativeEmptyIdRight =
      forAll randomEvent $ \e ->
        (e <|> noEvent ) == e

-- * Internal utilities for event construction

-- | maybeToEvent
testMaybeToEvent :: Property
testMaybeToEvent =
  forAll randomMaybe $ \m ->
    event Nothing Just (maybeToEvent m) == m
-- * Utility functions similar to those available for Maybe

-- | event
testEvent :: TestTree
testEvent = testGroup "event"
    [ testProperty "event > NoEvent" testEventNoEvent
    , testProperty "event > Event"   testEventEvent
    ]
  where
    testEventNoEvent :: Property
    testEventNoEvent =
      forAll randomValue $ \x ->
        forAllBlind randomFunction $ \f ->
          event x f noEvent == x

    testEventEvent :: Property
    testEventEvent =
      forAll randomValue $ \x ->
        forAllBlind randomFunction $ \f ->
          forAll randomValue $ \y ->
            event x f (Event y) == f y

-- | fromEvent
testFromEvent :: Property
testFromEvent =
  forAll randomValue $ \x ->
    fromEvent (Event x) == x

-- | isEvent
testIsEvent :: Property
testIsEvent =
  forAll randomValue $ \x ->
    isEvent (Event x) && not (isEvent noEvent)

-- | isNoEvent
testIsNoEvent :: Property
testIsNoEvent =
  forAll randomValue $ \x ->
    isNoEvent noEvent && not (isNoEvent (Event x))

-- * Event tagging

-- | tag
testTag :: Property
testTag =
  forAll randomValue $ \x ->
    forAll randomValue $ \y ->
      tag noEvent y == noEvent && tag (Event x) y == Event y

-- | tagWith
testTagWith :: Property
testTagWith =
  forAll randomValue $ \x ->
    forAll randomValue $ \y ->
      tagWith y noEvent == noEvent && tagWith y (Event x) == Event y

-- | attach
testAttach :: Property
testAttach =
  forAll randomValue $ \x ->
    forAll randomValue $ \y ->
      attach (noEvent :: Event Integer) x == noEvent
        && attach (Event x) y == Event (x, y)

-- * Event merging (disjunction) and joining (conjunction)

-- | lMerge
testLMerge :: Property
testLMerge =
  forAll randomValue $ \x ->
    forAll randomEvent $ \e ->
      lMerge (Event x) e == Event x && lMerge noEvent e == e

-- | rMerge
testRMerge :: Property
testRMerge =
  forAll randomValue $ \x ->
    forAll randomEvent $ \e ->
      rMerge e (Event x) == Event x && rMerge e noEvent == e

-- | merge
testMerge :: Property
testMerge =
  forAll randomEvent $ \e1 ->
    forAll randomEvent $ \e2 ->
      (isEvent e1 && isEvent e2)
      || merge e1 e2 == lMerge e1 e2

-- | mergeBy
testMergeBy :: TestTree
testMergeBy = testGroup "mergeBy"
    [ testProperty "mergeBy > events"    testMergeByEvents
    , testProperty "mergeBy > no events" testMergeByNoEvent
    ]
  where
    testMergeByEvents :: Property
    testMergeByEvents =
      forAll randomValue $ \x ->
        forAll randomValue $ \y ->
          forAllBlind randomFunction2 $ \f ->
            mergeBy f (Event x) (Event y) == Event (f x y)

    testMergeByNoEvent :: Property
    testMergeByNoEvent =
      forAll randomEvent $ \e ->
        forAllBlind randomFunction2 $ \f ->
          (mergeBy f noEvent e == e) && (mergeBy f e noEvent == e)

-- | mapMerge
testMapMerge :: TestTree
testMapMerge = testGroup "testMapMerge"
    [ testProperty "mapMerge > left event"  testMapMergeL
    , testProperty "mapMerge > right event" testMapMergeR
    , testProperty "mapMerge > both events" testMapMergeBoth
    ]
  where
    testMapMergeL :: Property
    testMapMergeL =
      forAllBlind randomFunction $ \f ->
        forAllBlind randomFunction $ \g ->
          forAllBlind randomFunction2 $ \h ->
            forAll randomEvent $ \e1 ->
              mapMerge f g h e1 NoEvent == (f <$> e1)

    testMapMergeR :: Property
    testMapMergeR =
      forAllBlind randomFunction $ \f ->
        forAllBlind randomFunction $ \g ->
          forAllBlind randomFunction2 $ \h ->
            forAll randomEvent $ \e2 ->
              mapMerge f g h NoEvent e2 == (g <$> e2)

    testMapMergeBoth :: Property
    testMapMergeBoth =
      forAllBlind randomFunction $ \f ->
        forAllBlind randomFunction $ \g ->
          forAllBlind randomFunction2 $ \h ->
            forAll randomValue $ \e1 ->
              forAll randomValue $ \e2 ->
                mapMerge f g h (Event e1) (Event e2)
                  == (h <$> Event e1 <*> Event e2)

-- | mergeEvents
testMergeEvents :: Property
testMergeEvents = forAll randomEvents $ \es ->
    mergeEvents es == fstEvent es
  where
    fstEvent = safeHead NoEvent . filter isEvent

    safeHead x []     = x
    safeHead _ (e:es) = e

-- | catEvents
testCatEvents :: Property
testCatEvents = forAll randomEvents $ \es ->
    catEvents es == catEventsModel es
  where
    catEventsModel es = if null es' then NoEvent else Event es'
      where
        es' = map fromEvent $ filter isEvent es

-- | joinE
testJoinE :: Property
testJoinE =
  forAll randomEvent $ \e1 ->
    forAll randomEvent $ \e2 ->
      joinE e1 e2 == ((,) <$> e1 <*> e2)

-- | splitE
testSplitE :: Property
testSplitE =
  forAll randomEventPair $ \e ->
    splitE e == (fst <$> e, snd <$> e)

-- * Event filtering

-- | filterE
testFilterE :: Property
testFilterE =
    forAllBlind randomPred $ \f ->
      forAll randomEvent $ \e ->
        filterE f e == filterModel f e
  where
    filterModel :: (a -> Bool) -> Event a -> Event a
    filterModel f e =
        case f <$> e of
          Event True -> e
          _          -> NoEvent

-- | mapFilterE
testMapFilterE :: Property
testMapFilterE =
  forAllBlind randomFunctionMaybe $ \f ->
    forAll randomEvent $ \e ->
      join ((maybeToEvent . f) <$> e) == mapFilterE f e

-- | gate
testGate :: Property
testGate =
  forAll randomEvent $ \e ->
    forAll randomBool $ \b ->
      gate e b == filterE (const b) e

-- * Arbitrary value generation

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = oneof [ return NoEvent
                    , do x <- arbitrary
                         return $ Event x
                    ]

randomValue :: Gen Integer
randomValue = arbitrary

randomBool :: Gen Bool
randomBool = arbitrary

randomMaybe :: Gen (Maybe Integer)
randomMaybe = arbitrary

randomEvent :: Gen (Event Integer)
randomEvent = arbitrary

randomEventPair :: Gen (Event (Integer, Integer))
randomEventPair = arbitrary

randomEvents :: Gen [Event Integer]
randomEvents = arbitrary

randomEventFunction :: Gen (Event Integer -> Event Integer)
randomEventFunction = do
  def <- arbitrary
  f   <- applyFun <$> arbitrary
  return $ event def f

randomEventFunctionIn :: Gen (Event (Integer -> Integer))
randomEventFunctionIn =
  oneof [ return noEvent
        , Event <$> applyFun <$> arbitrary
        ]

randomEventAction :: Gen (Integer -> Event Integer)
randomEventAction = arbitrary

randomFunction :: Gen (Integer -> Integer)
randomFunction = arbitrary

randomFunction2 :: Gen (Integer -> Integer -> Integer)
randomFunction2 = arbitrary

randomFunctionMaybe :: Gen (Integer -> Maybe Integer)
randomFunctionMaybe = arbitrary

randomPred :: Gen (Integer -> Bool)
randomPred = arbitrary

-- * Auxiliary

-- | Logical implication.
(-->) :: Bool -> Bool -> Bool
(-->) x y = x <= y

-- | Logical equivalence (iff).
(<->) :: Bool -> Bool -> Bool
(<->) = (==)
