module YampaSDL where

import Data.IORef
import FRP.Yampa       as Yampa
import Graphics.UI.SDL as SDL

type TimeRef = IORef Int

yampaSDLTimeInit :: IO TimeRef
yampaSDLTimeInit = do
  timeRef <- newIORef (0 :: Int)
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef
  _       <- yampaSDLTimeSense timeRef
  return timeRef

-- | Updates the time in an IO Ref and returns the time difference
updateTime :: IORef Int -> Int -> IO Int
updateTime timeRef newTime = do
  previousTime <- readIORef timeRef
  writeIORef timeRef newTime
  return (newTime - previousTime)

yampaSDLTimeSense :: IORef Int -> IO Yampa.DTime
yampaSDLTimeSense timeRef = do
  -- Get time passed since SDL init
  newTime <- fmap fromIntegral SDL.getTicks

  -- Obtain time difference
  dt <- updateTime timeRef newTime
  let dtSecs = fromIntegral dt / 100
  return dtSecs
