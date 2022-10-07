-- |
-- Description : Test cases for tasks (Task)
-- Copyright   : (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson
-- Very rudimentary testing of Task.

module Test.FRP.Yampa.Task
    ( tests )
  where

import Control.Monad (when, forever)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import FRP.Yampa as Yampa
import FRP.Yampa.Task

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Task"
  [ testProperty "tasks (fixed)" (property $ task_t0 ~= task_t0r)
  , testProperty "tasks (fixed)" (property $ task_t1 ~= task_t0r)  -- Intentionally! task_t0 = task_t1!
  , testProperty "tasks (fixed)" (property $ task_t2 ~= task_t2r)
  , testProperty "tasks (fixed)" (property $ task_t3 ~= task_t3r)
  , testProperty "tasks (fixed)" (property $ task_t4 ~= task_t4r)
  , testProperty "tasks (fixed)" (property $ task_t5 ~= task_t5r)
  , testProperty "tasks (fixed)" (property $ task_t6 ~= task_t6r)
  , testProperty "tasks (fixed)" (property $ task_t7 ~= task_t7r)
  , testProperty "tasks (fixed)" (property $ task_t8 ~= task_t8r)
  ]

-- * The Task type

task_t0 = testSF1 (runTask (do
                               mkTask (localTime
                                       &&&(localTime >>> arr (>=5.0) >>> edge))
                               x <- snapT
                               return (x * 2.0))
                  )

task_t0r =
  [ Left 0.0,   Left 0.25,  Left 0.5,   Left 0.75,  Left 1.0
  , Left 1.25,  Left 1.5,   Left 1.75,  Left 2.0,   Left 2.25
  , Left 2.5,   Left 2.75,  Left 3.0,   Left 3.25,  Left 3.5
  , Left 3.75,  Left 4.0,   Left 4.25,  Left 4.5,   Left 4.75
  , Right 40.0, Right 40.0, Right 40.0, Right 40.0, Right 40.0
  ]

task_t1 = testSF1 (runTask (do
                               mkTask (localTime
                                       &&& (localTime>>>arr (>=5.0) >>> edge))
                               return ()   -- No time should pass!
                               return ()   -- No Time should pass!
                               snapT       -- No time should pass!
                               snapT       -- No time should pass!
                               x <- snapT
                               return (x * 2.0))
                  )

task_t2 = testSF1 (runTask (do
                               sleepT 1.51 42.0
                               x <- snapT
                               y <- snapT
                               sleepT 1.51 x
                               if x == y
                                 then sleepT 1.51 (x * 2)
                                 else sleepT 0.51 (x * 3)
                           )
                  )

task_t2r =
  [ Left 42.0, Left 42.0, Left 42.0, Left 42.0  -- 0.0 s
  , Left 42.0, Left 42.0, Left 42.0, Left 7.0   -- 1.0 s
  , Left 7.0,  Left 7.0,  Left 7.0,  Left 7.0   -- 2.0 s
  , Left 7.0,  Left 7.0,  Left 14.0, Left 14.0  -- 3.0 s
  , Left 14.0, Left 14.0, Left 14.0, Left 14.0  -- 4.0 s
  , Left 14.0, Right (),  Right (),  Right ()   -- 5.0 s
  , Right ()
  ]

task_t3 = testSF1 (runTask (do
                              c <- sawtooth `timeOut` 3.49
                              case c of
                                Nothing -> sleepT 1.51 (-10.0)
                                Just x  -> sleepT 1.51 x
                          )
                 )
  where
    sawtooth =
      forever ((mkTask (constant 2.0 >>> integral &&& never))
               `timeOut` 1.5)

task_t3r :: [Either Double ()]
task_t3r =
  [ Left 0.0,     Left 0.5,     Left 1.0,     Left 1.5      -- 0.0 s
  , Left 2.0,     Left 2.5,     Left 0.0,     Left 0.5      -- 1.0 s
  , Left 1.0,     Left 1.5,     Left 2.0,     Left 2.5      -- 2.0 s
  , Left 0.0,     Left 0.5,     Left (-10.0), Left (-10.0)  -- 3.0 s
  , Left (-10.0), Left (-10.0), Left (-10.0), Left (-10.0)  -- 4.0 s
  , Left (-10.0), Right (),     Right (),     Right ()      -- 5.0 s
  , Right ()
  ]

task_t4 = testSF1 (runTask (do
                              c <- sawtooth `timeOut` 3.49
                              case c of
                                Nothing -> sleepT 1.51 (-10.0)
                                Just x  -> sleepT 1.51 x
                           )
                  )
  where
    sawtooth = do
      for 1 (+1) (<=2)
          ((mkTask (constant 2.0 >>> integral &&& never))
           `timeOut` 1.5)
      return (-42.0)

task_t4r :: [Either Double ()]
task_t4r =
  [ Left 0.0,     Left 0.5,     Left 1.0,     Left 1.5      -- 0.0 s
  , Left 2.0,     Left 2.5,     Left 0.0,     Left 0.5      -- 1.0 s
  , Left 1.0,     Left 1.5,     Left 2.0,     Left 2.5      -- 2.0 s
  , Left (-42.0), Left (-42.0), Left (-42.0), Left (-42.0)  -- 3.0 s
  , Left (-42.0), Left (-42.0), Left (-42.0), Right ()      -- 4.0 s
  , Right (),     Right (),     Right (),     Right ()      -- 5.0 s
  , Right ()
  ]

task_t5 = testSF1 (runTask (do
                              x<-(sawtoothCycle>>snapT) `repeatUntil` (>=20.0)
                              y<-snapT
                              return (x == y)
                           )
                  )
  where
    sawtoothCycle = mkTask (constant 2.0 >>> integral &&& after 1.5 ())

task_t5r :: [Either Double Bool]
task_t5r =
  [ Left 0.0, Left 0.5, Left 1.0, Left 1.5  -- 0.0 s, 0 - 3
  , Left 2.0, Left 2.5, Left 0.0, Left 0.5  -- 1.0 s, 4 - 7
  , Left 1.0, Left 1.5, Left 2.0, Left 2.5  -- 2.0 s, 8 - 11
  , Left 0.0, Left 0.5, Left 1.0, Left 1.5  -- 3.0 s, 12 - 15
  , Left 2.0, Left 2.5, Left 0.0, Left 0.5  -- 4.0 s, 16 - 19
  , Left 1.0, Left 1.5, Left 2.0, Left 2.5  -- 5.0 s, 20 - 23
  , Right True
  ]

task_t6 = testSF1 $ runTask $ do
    x <- ((sawtoothCycle >> snapT) `repeatUntil` (>=20.0))
         `abortWhen` (localTime >>> arr (>=3.51) >>> edge)
    y <- snapT
    return (x,y)
  where
    sawtoothCycle = mkTask (constant 2.0 >>> integral &&& after 1.5 ())

task_t6r :: [Either Double (Either Double (), Double)]
task_t6r =
  [ Left 0.0, Left 0.5, Left 1.0, Left 1.5              -- 0.0 s, 0 - 3
  , Left 2.0, Left 2.5, Left 0.0, Left 0.5              -- 1.0 s, 4 - 7
  , Left 1.0, Left 1.5, Left 2.0, Left 2.5              -- 2.0 s, 8 - 11
  , Left 0.0, Left 0.5, Left 1.0, Right (Right (),15.0) -- 3.0 s, 12 - 15
  , Right (Right (),15.0), Right (Right (),15.0)        -- 4.0 s, 16, 17
  , Right (Right (),15.0), Right (Right (),15.0)        -- 4.5 s, 18, 19
  , Right (Right (),15.0), Right (Right (),15.0)        -- 5.0 s, 20, 21
  , Right (Right (),15.0), Right (Right (),15.0)        -- 5.5 s, 22, 23
  , Right (Right (),15.0)
  ]

task_t7 = testSF1 $ runTask $ do
    x <- ((sawtoothCycle >> snapT) `repeatUntil` (>=20.0))
         `abortWhen` (localTime >>> arr (>=5.75) >>> edge)
    y <- snapT
    return (x,y)
  where
    sawtoothCycle = mkTask (constant 2.0 >>> integral &&& after 1.5 ())

task_t7r :: [Either Double (Either Double (), Double)]
task_t7r =
    [ Left 0.0, Left 0.5, Left 1.0, Left 1.5              -- 0.0 s, 0 - 3
    , Left 2.0, Left 2.5, Left 0.0, Left 0.5              -- 1.0 s, 4 - 7
    , Left 1.0, Left 1.5, Left 2.0, Left 2.5              -- 2.0 s, 8 - 11
    , Left 0.0, Left 0.5, Left 1.0, Left 1.5              -- 3.0 s, 12 - 15
    , Left 2.0, Left 2.5, Left 0.0, Left 0.5              -- 4.0 s, 16 - 19
    , Left 1.0, Left 1.5, Left 2.0, Right (Right (),23.0) -- 5.0 s, 20 - 23
    , Right (Right (),23.0)
    ]

task_t8 = testSF1 $ runTask $ do
    x <- ((sawtoothCycle >> snapT) `repeatUntil` (>=20.0))
         `abortWhen` (localTime >>> arr (>=5.76) >>> edge)
    y <- snapT
    return (x,y)
  where
    sawtoothCycle = mkTask (constant 2.0 >>> integral &&& after 1.5 ())

-- Since abortWhen uses lMergeEvent, the terminating event of the task
-- gets priority over the aborting event.
task_t8r :: [Either Double (Either Double (), Double)]
task_t8r =
    [ Left 0.0, Left 0.5, Left 1.0, Left 1.5  -- 0.0 s, 0 - 3
    , Left 2.0, Left 2.5, Left 0.0, Left 0.5  -- 1.0 s, 4 - 7
    , Left 1.0, Left 1.5, Left 2.0, Left 2.5  -- 2.0 s, 8 - 11
    , Left 0.0, Left 0.5, Left 1.0, Left 1.5  -- 3.0 s, 12 - 15
    , Left 2.0, Left 2.5, Left 0.0, Left 0.5  -- 4.0 s, 16 - 19
    , Left 1.0, Left 1.5, Left 2.0, Left 2.5  -- 5.0 s, 20 - 23
    , Right (Left 24.0,24.0)
    ]

-- * Auxiliary

-- | Repeat m until result satisfies the predicate p
repeatUntil :: Monad m => m a -> (a -> Bool) -> m a
m `repeatUntil` p = m >>= \x -> if not (p x) then repeatUntil m p else return x

-- | C-style for-loop.
--
-- Example:
--
-- >>> for 0 (+1) (>=10) ...
for :: Monad m => a -> (a -> a) -> (a -> Bool) -> m b -> m ()
for i f p m = when (p i) $ m >> for (f i) f p m
