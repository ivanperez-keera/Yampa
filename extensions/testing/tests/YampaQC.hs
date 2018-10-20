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
module YampaQC where

------------------------------------------------------------------------------
import Data.Fixed

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Function

import FRP.Yampa as Yampa
import FRP.Yampa.EventS (snap)
import FRP.Yampa.Stream
import FRP.Yampa.QuickCheck
import FRP.Yampa.LTLFuture

------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testProperty "SF based on (**2) equal to SF on (^2))" prop_arr_law1
    , testProperty "Identity"                               prop_arr_id
    , testProperty "Arrow Naturality"                       prop_arr_naturality
    , testProperty "Naturality"                             prop_arr_naturality
    , testProperty "Basic > Identity (1)"                   prop_basic_identity_1
    , testProperty "Basic > Identity (2)"                   prop_basic_identity_2
    , testProperty "Basic > Constant"                       prop_basic_constant
    , testProperty "Basic > Initially"                      prop_basic_initially
    , testProperty "Basic > Time"                           prop_basic_time_increasing
    , testProperty "Basic > Time (fixed delay)"             prop_basic_time_fixed_delay
    , testProperty "Basic > localTime"                      prop_basic_localtime_increasing
    , testProperty "Basic > localTime (fixed delay)"        prop_basic_localtime_fixed_delay
    , testProperty "Collections > parB"                     prop_broadcast
    , testProperty "Arrows > Composition (1)"               prop_arrow_comp_1
    , testProperty "Arrows > Composition (2)"               prop_arrow_comp_2
    , testProperty "Arrows > Composition (3)"               prop_arrow_comp_3
    , testProperty "Delays > Zero delay"                    prop_delay_1
    , testProperty "Delays > Small delay"                   prop_delay_2
    -- FIXME: (iperez:) delay_t3 is not here because I can't understand it.
    -- Missing: delay t4 and t5
    , testProperty "Derivatives > Comparison with known derivative (1)" prop_derivative_1
    , testProperty "Derivatives > Comparison with known derivative (2)" prop_derivative_2
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
    ]

-- * Yampa laws

-- ** Arrow laws
prop_arr_law1 =
   forAll myStream (evalT $ prop_always_equal (arr (**2)) (arr (^2)))
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

prop_arr_id =
   forAll myStream (evalT $ prop_always_equal (arr id) identity)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

-- Yampa's internal test cases

-- prop :: SF a b -> (a -> b ->
prop (a,b) = Prop ((identity &&& a) >>^ uncurry b)

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

-- Yampa's Basic SF builders
prop_basic_identity_1 =
   forAll myStream $ evalT $ Always $ prop (sf, pred)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream
       sf   = identity
       pred = (==)

prop_basic_identity_2 =
   forAll myStream (evalT $ prop_always_equal identity (arr id))
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

prop_basic_constant =
   forAll myStream $ evalT $ Always $ prop (sf, pred)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sf   = constant 42.0
       pred = const (== 42.0)

prop_basic_initially =
   forAll myStream $ evalT $ prop (sf, pred)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sf   = initially 42.0
       pred = const (== 42.0)

-- | Starting with an accumulator of -1, it gets the local
--   time and outputs the time and the accumulator, updating
--   the latter with the local time at every iteration.
--   The predicate checks whether the time is always strictly
--   greater than the acc.
prop_basic_time_increasing =
   forAll myStream $ evalT $ Always $ prop (sf, pred)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sf   :: SF a (Time, Time)
       sf   = loopPre (-1 :: Time) sfI

       sfI :: SF (a,Time) ((Time, Time), Time)
       sfI =  (time *** identity) >>> arr resort

       resort :: (Time, Time) -> ((Time,Time),Time)
       resort (newT, oldT) = ((newT, oldT), newT)

       pred :: a -> (Time, Time) -> Bool
       pred _ (t,o) = (t > o)

prop_basic_time_fixed_delay =
   forAll myStream $ evalT $
         Always (prop (sf25msec, const (== d)))

 where myStream :: Gen (SignalSampleStream Float)
       myStream = fixedDelayStream d

       sf25msec = time >>> stepDiff (-d)

       d :: Time
       d = 0.25

prop_basic_localtime_increasing =
   forAll myStream $ evalT $ Always $ prop (sf, const (uncurry (>)))
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sf   :: SF a (Time, Time)
       sf   = loopPre (-1 :: Time) sfI

       sfI :: SF (a,Time) ((Time, Time), Time)
       sfI =  (localTime *** identity) >>> arr resort

       resort :: (Time, Time) -> ((Time,Time),Time)
       resort (newT, oldT) = ((newT, oldT), newT)

prop_basic_localtime_fixed_delay =
   forAll myStream $ evalT $
         Always (prop (sf25msec, const (== d)))

 where myStream :: Gen (SignalSampleStream Float)
       myStream = fixedDelayStream d

       sf25msec = time >>> stepDiff (-d)

       d :: Time
       d = 0.25

-- Par with broadcast (collection-oriented combinators)
-- TODO: Add integral to the list of SFs being tested
prop_broadcast =
   forAll myStream $ evalT $ Always $ prop (sf, pred)
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sf   = parB [identity, (arr (+1))]
       pred = (\x [y,z] -> x == y && (x + 1) == z)

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

-- Delaying

-- | Delaying by 0.0 has no effect
prop_delay_1 =
   forAll myStream $ evalT $ prop_always_equal sfDelayed sf
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sfDelayed = delay 0.0 undefined >>> sf
       sf = arr (+1)

-- | Delaying input signal by a small amount will fill in the "blank" signal
--   with the given value, which will become also the sample at the initial
--   time.
prop_delay_2 =
   forAll myStream $ evalT $
     (prop (sfDelayed, (\x y -> y == initialValue)))
 where myStream :: Gen (SignalSampleStream Float)
       myStream = uniDistStream

       sfDelayed = delay 0.0001 initialValue

       initialValue = 17

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

prop_derivative_1 =
   forAll myStream $ evalT $
     Next $ Always $ prop ((sfDer &&& sfDerByHand), const close)

  where myStream :: Gen (SignalSampleStream Double)
        myStream = fixedDelayStreamWith (\t -> sin(2 * pi * t)) der_step

        sfDer :: SF Time Time
        sfDer = derivative

        sfDerByHand = localTime >>> arr (\t -> (2 * pi * cos (2 * pi * t)))

        close (x,y) = abs (x-y) < 0.05

prop_derivative_2 =
   forAll myStream $ evalT $
     Next $ Always $ prop ( sfDer &&& sfDerByHand
                          , const close)

  where
    myStream :: Gen (SignalSampleStream Double)
    myStream = fixedDelayStream der_step

    sfDer :: SF Time Time
    sfDer = localTime
              >>> arr (\t -> sin(2*pi*t))
                >>> derivative

    sfDerByHand = localTime
                    >>> arr (\t -> 2*pi*cos (2*pi*t))

    close (x,y) = abs (x-y) < 0.05

der_step = 0.001

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
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> pred)
 where sf1 = arr id >>> integral
       sf2 = integral
       pred = arr $ uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_id_2 =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> pred)
 where sf1 = integral >>> arr id
       sf2 = integral
       pred = arr $ uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_assoc =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> pred)
 where sf1 = (integral >>> arr (*0.5)) >>> integral
       sf2 = integral >>> (arr (*0.5) >>> integral)
       pred = arr $ uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_arr_comp =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> pred)
 where sf1 = (arr ((*2.5) . (+3.0)))
       sf2 = (arr (+3.0) >>> arr (*2.5))
       pred = arr (uncurry (==))

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_first_3 =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> arr pred)
 where sf1 = (arr dup >>> first (arr (*2.5)))
       sf2 = (arr dup >>> arr (fun_prod (*2.5) id))
       pred = uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_first_distrib =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> arr pred)
 where sf1 = (arr dup >>> (first (integral >>> arr (+3.0))))
       sf2 = (arr dup >>> (first integral >>> first (arr (+3.0))))
       pred = uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_first_id_comm =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> arr pred)
 where sf1 = (arr dup >>> (first integral>>>arr (fun_prod id (+3.0))))
       sf2 = (arr dup >>> (arr (fun_prod id (+3.0))>>>first integral))
       pred = uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_arrow_first_nested =
   forAll myStream $ evalT $ Always $ Prop ((sf1 &&& sf2) >>> arr pred)
 where sf1 = (arr (\x -> ((x,x),())) >>> (first (first integral) >>> arr assoc))
       sf2 = (arr (\x -> ((x,x),())) >>> (arr assoc >>> first integral))

       pred = uncurry (==)

       myStream :: Gen (SignalSampleStream Double)
       myStream = uniDistStream

prop_switch_t1 =
  forAll myStream $ evalT $
    Always $ Prop ((switch_t1rec 42.0 &&& switch_tr) >>> arr same)

  where myStream :: Gen (SignalSampleStream Double)
        myStream = fixedDelayStreamWith f 1.0
        f dt = l!!(floor dt)
        l = [1.0, 1.0, 1.0,
             2.0,
             3.0, 3.0,
             4.0, 4.0, 4.0,
             5.0,
             6.0, 6.0,
             7.0, 7.0, 7.0,
             8.0]
             ++ repeat 9.0

        same = (uncurry (==))

-- Outputs current input, local time, and the value of the initializing
-- argument until some time has passed (determined by integrating a constant),
-- at which point an event occurs.
switch_t1a :: Double -> SF Double ((Double,Double,Double), Event ())
switch_t1a x = (arr dup >>> second localTime >>> arr (\(a,t) -> (a,t,x)))
           &&& (constant 0.5
                    >>> integral
                    >>> (arr (>= (2.0 :: Double)) -- Used to work with no sig.
                    >>> edge))

-- This should raise an event IMMEDIATELY: no time should pass.
switch_t1b :: b -> SF a ((Double,Double,Double), Event a)
switch_t1b _ = constant (-999.0,-999.0,-999.0) &&& snap

switch_t1rec :: Double -> SF Double (Double,Double,Double)
switch_t1rec x =
    switch (switch_t1a x) $ \x ->
    switch (switch_t1b x) $ \x ->
    switch (switch_t1b x) $
    switch_t1rec

switch_tr :: SF Double (Double, Double, Double)
switch_tr = proc (a) -> do
   t <- localTime -< ()
   let mt = fromIntegral $ floor (mod' t 4.0)
       v  = case floor (t / 4.0) of
             0 -> 42.0
             1 -> 3.0
             2 -> 4.0
             3 -> 7.0
             _ -> 9.0
   returnA -< (a, mt, v)

infiniteSwitch sf1 sf2 input =
   switched (evalAtZero sf1 input) /= switched (evalAtZero sf2 input)
  where switched = isEvent . snd . fst

switch1 = switch (simpleF)
                 (\_ -> switch1)

simpleF = arr id &&& cond
 where cond = arr (const (Event ()))

delayedF = arr id &&& cond
 where cond = after 1.5 (Event ())


-- * Generic SF predicate building functions

-- | Compares two SFs, resulting in true if they are always equal
prop_always_equal sf1 sf2 =
    Always $ Prop ((sf1 &&& sf2) >>> arr sameResult)
  where sameResult = uncurry (==)

prop_arr_no_change f xs =
     samples (fst (evalSF (arr f) xs)) == map f (samples xs)

-- | Compares two SFs, returning true if they are close enough
prop_always_similar margin sf1 sf2 =
  Always (Prop ((sf1 &&& sf2) >>> arr similar))
  where similar (x,y) = abs (x-y) <= margin

sfMeasureIncrement :: Num b => b -> SF a b -> SF a b
sfMeasureIncrement init sf = loopPre init sf'
 where sf' = (sf *** identity) >>> arr (\(n, o) -> (n - o, n))

fun_prod f g = \(x,y) -> (f x, g y)

assoc :: ((a,b),c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))

assocInv :: (a,(b,c)) -> ((a,b),c)
assocInv (a,(b,c)) = ((a,b),c)
