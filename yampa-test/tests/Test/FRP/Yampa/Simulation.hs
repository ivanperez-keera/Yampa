-- |
-- Description : Test cases for FRP.Yampa.Simulation
-- Copyright   : (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- Authors     : Antony Courtney, Henrik Nilsson
module Test.FRP.Yampa.Simulation
    ( tests
    )
  where

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, writeIORef, readIORef)

import FRP.Yampa as Yampa

import TestsCommon

tests :: TestTree
tests = testGroup "Regression tests for FRP.Yampa.Simulation"
  [ testProperty "react (fixed)"    (property $ react_t0 ~= react_t0r)
  , testProperty "embed (0, fixed)" (property $ embed_t0 ~= embed_t0r)
  , testProperty "embed (1, fixed)" (property $ embed_t1 ~= embed_t1r)
  ]

-- * Reactimation

react_t0 :: [(Double, Double)]
react_t0 = unsafePerformIO $ do
  countr   <- newIORef undefined
  inputr   <- newIORef undefined
  outputsr <- newIORef []
  let init = do
        writeIORef countr 1
        let input0 = 0.0
        writeIORef inputr input0
        return input0
      sense _ = do
        count <- readIORef countr
        if count >= 5
          then do
            writeIORef countr 1
            input <- readIORef inputr
            let input' = input + 0.5
            writeIORef inputr input'
            return (0.1, Just input')
          else do
            writeIORef countr (count + 1)
            return (0.1, Nothing)
      actuate _ output = do
        outputs <- readIORef outputsr
        writeIORef outputsr (output : outputs)
        input <- readIORef inputr
        return (input > 5.0)
  reactimate init sense actuate (arr dup >>> second integral)
  outputs <- readIORef outputsr
  return (take 25 (reverse outputs))

react_t0r :: [(Double, Double)]
react_t0r =
  [ (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00), (0.0,0.00)
  , (0.5,0.00), (0.5,0.05), (0.5,0.10), (0.5,0.15), (0.5,0.20)
  , (1.0,0.25), (1.0,0.35), (1.0,0.45), (1.0,0.55), (1.0,0.65)
  , (1.5,0.75), (1.5,0.90), (1.5,1.05), (1.5,1.20), (1.5,1.35)
  , (2.0,1.50), (2.0,1.70), (2.0,1.90), (2.0,2.10), (2.0,2.30)
  ]

-- * Embedding

embed_ratio :: SF a Double
embed_ratio = switch (constant 1.0 &&& after 5.0 ()) $ \_ ->
              switch (constant 0.0 &&& after 5.0 ()) $ \_ ->
              constant 3.0

embed_sf :: SF a Double
embed_sf = localTime >>> integral

embed_t0 = take 20 $ embed (embed_ratio
                            >>> embedSynch embed_sf
                                           (deltaEncode 0.01 (repeat ())))
                           (deltaEncode 1.0 (repeat ()))

embed_t0r =
  [   0.0000,   0.4851,   1.9701,    4.4850,   7.9800
  ,   7.9800,   7.9800,   7.9800,    7.9800,   7.9800
  ,  24.4650,  49.9500,  84.4350,  127.9200, 180.2151
  , 241.6701, 312.1251, 391.5801, 480.03510, 577.4901
  ]

embed_t1 = take 20 $ embed (embed_ratio
                            >>> embedSynch embed_sf
                                           (deltaEncode 0.5 (replicate 30 ())))
                           (deltaEncode 1.0 (repeat ()))

embed_t1r =
  [   0.00,   0.25,   1.50,   3.75,   7.00
  ,   7.00,   7.00,   7.00,   7.00,   7.00
  ,  22.75,  47.50,  81.25, 101.50, 101.50
  , 101.50, 101.50, 101.50, 101.50, 101.50
  ]
