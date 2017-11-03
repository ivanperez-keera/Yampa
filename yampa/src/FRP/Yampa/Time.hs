{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------------------
-- |
-- Module      : FRP.Yampa.Time
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- SF primitives that producing the current running time.
--
-- Time is global for an 'SF', so, every constituent 'SF' will use the same
-- global clock. However, when used in combination with
-- 'FRP.Yampa.Switches.switch'ing, the SF switched into will be started at the
-- time of switching, so any reference to 'localTime' or 'time' from that 'SF'
-- will count using the time of switching as the start time.
--
-- Take also into account that, because 'FRP.Yampa.Integration.derivative' is
-- the derivative of a signal /over time/, differentiating 'localTime' will
-- always produce the value one (@1@). If you really, really, really need to
-- know the time delta, and need to abandon the hybrid\/FRP abstraction, see
-- 'FRP.Yampa.Integration.iterFrom'.
module FRP.Yampa.Time
    ( localTime
    , time
    , timeTransform
    , timeTransformSF
    , revSwitch
    , alwaysForward
    , clocked
    , forgetPast
    , limitHistory
    )
  where

-- External imports
import Control.Arrow ((>>>))

-- Internal imports
import FRP.Yampa.Basic        (constant)
import FRP.Yampa.Diagnostics  (usrErr)
import FRP.Yampa.Event        (Event (..))
import FRP.Yampa.Integration  (integral)
import FRP.Yampa.InternalCore (DTime, SF (SF), SF' (SF'), Time, sfTF, sfTF')

-- | Outputs the time passed since the signal function instance was started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: SF a Time
time = localTime

-- ** Time transformations (run SFs slower/faster)

-- NOTE: These versions are not optimized.

timeTransform :: (DTime -> DTime) -> SF a b -> SF a b
timeTransform transform sf = SF tf
 where tf a = let (sf', b) = (sfTF sf) a
                  sf''     = timeTransformF transform sf'
              in (sf'', b)

timeTransformF :: (DTime -> DTime) -> SF a b -> SF a b
timeTransformF transform sf = SF' tf
 where tf dt a = let dt'      = transform dt
                     (sf', b) = (sfTF' sf) dt' a
                     sf''     = timeTransformF transform sf'
                 in if dt' <= 0
                          then usrErr "AFRP" "timeTransform" "The time cannot be negative"
                          else (sf'', b)

timeTransformSF :: SF a (DTime -> DTime)
                -> SF a b
                -> SF a b
timeTransformSF transformSF sf = SF tf
 where tf a = let (transformSFF, _) = (sfTF transformSF) a
                  (sf', b) = (sfTF sf) a
                  sf''     = timeTransformSFF transformSFF sf'
              in (sf'', b)

timeTransformSFF :: SF' a (DTime -> DTime) -> SF' a b -> SF' a b
timeTransformSFF transformSF sf = SF' tf
 where tf dt a = let (transformSF', transform) = (sfTF' transformSF) dt a
                     dt'                           = transform dt
                     (sf', b)                      = (sfTF' sf) dt' a
                     sf''                          = timeTransformSFF transformSF' sf'
                 in (sf'', b)

reverseTime :: SF a b -> SF a b
reverseTime = timeTransform ((-1)*)

type History a = [(Time, a)]

lastSample :: History a -> (Time, a)
lastSample as = last as

cache :: ((Time, a) -> Maybe (Time, a) -> Time -> a) -> SF a a
cache interpolate = SF tf
  where
    tf a = (cache' interpolate ([(0, a)]) 0, a)

cache' :: ((Time, a) -> Maybe (Time, a) -> Time -> a) -> History a -> Time -> SF' a a
cache' interpolate history globalTime = SF' tf
  where
    tf dt a | dt == 0 = (cache' interpolate history globalTime, snd $ lastSample history)
            | dt <  0 = let globalTime' = globalTime + dt
                            sample = sampleAt interpolate history globalTime'
                            history' = discardAfter history globalTime'
                            history'' = addSample  history' globalTime' sample
                        in (cache' interpolate history'' globalTime', sample)

sampleAt :: ((Time, a) -> Maybe (Time, a) -> Time -> a) -> History a -> Time -> a
sampleAt interpolate history time = sampleAt' interpolate time (head history) (tail history)
sampleAt' interpolate time (t0, a0) [] = interpolate (t0, a0) Nothing time
sampleAt' interpolate time (t0, a0) ((t1,a1):hs)
  | time >= t0 && time <= t1 = interpolate (t0, a0) (Just (t1, a1)) time
  | otherwise                = sampleAt' interpolate time (t1,a1) hs

discardAfter :: History a -> Time -> History a
discardAfter history time = filter (\(t, a) -> t <= time) history

addSample :: History a -> Time -> a -> History a
addSample history time sample = addSample' time sample history

addSample' :: Time -> a -> History a -> History a
addSample' time sample [] = [(time, sample)]
addSample' time sample h@((t1,a1):hs)
  | t1 == time = (time, sample):hs
  | t1 >  time = (time, sample):h
  | otherwise  = (t1,a1) : addSample' time sample hs

revSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
revSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            case tf10 a0 of
                (sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
                (sf1, (_,  Event c0)) -> switchingPoint sf1 k (sfTF (k c0) a0)

        switchingPoint :: SF' a (b, Event c) -> (c -> SF a b) -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint sf1 k (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = if | dt < 0  -> sfTF' (switchAux sf1 k) dt a
                                           -- let (sf1', b') = sfTF' sf1 dt a
                                           -- in (switchAux sf1' k, b')
                              | dt > 0  -> switchingPoint' sf1 k dt (sfTF' sfN' dt a)
                              | dt == 0 -> switchingPoint sf1 k (sfN', b)

        switchingPoint' :: SF' a (b, Event c) -> (c -> SF a b) -> DTime -> (SF' a b, b) -> (SF' a b, b)
        switchingPoint' sf1 k accumDT (sfN', b) = (sf', b)
          where sf' = SF' tf'
                tf' dt a = let dt' = dt + accumDT
                           in if | dt < 0  -> if | dt' < 0  -> sfTF' (switchAux sf1 k) dt' a
                                                 | dt' > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                                 | dt' == 0 -> switchingPoint' sf1 k accumDT (sfN', b)
                                 | dt > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
                                 | dt == 0 -> switchingPoint' sf1 k accumDT (sfN', b)


        switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
        switchAux sf1                          k = SF' tf
            where
                tf dt a =
                    case (sfTF' sf1) dt a of
                        (sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
                        (_,    (_, Event c)) -> switchingPoint sf1 k (sfTF (k c) a)

checkpoint :: SF a (b, Event (), Event ()) -> SF a b
checkpoint sf = SF $ \a -> let (sf', (b, advance, reset)) = sfTF sf a
                           in case reset of
                                Event () -> error "loop"
                                NoEvent -> let pt = case advance of
                                                      Event () -> Left sf'
                                                      NoEvent  -> Right sf
                                           in (checkpoint' pt sf', b)

checkpoint' :: Either (SF' a (b, Event (), Event ())) (SF a (b, Event (), Event ()))
            -> (SF' a (b, Event (), Event ()))
            -> SF' a b
checkpoint' rstPt sf' = SF' $ \dt a -> let (sf'', (b, advance, reset)) = sfTF' sf' dt a
                                       in case reset of
                                            Event () -> case rstPt of
                                                          Left sf''' -> sfTF' (checkpoint' rstPt sf''') dt a
                                                          Right sf   -> sfTF (checkpoint sf) a
                                            NoEvent -> let pt = case advance of
                                                                  Event () -> Left sf''
                                                                  NoEvent -> rstPt
                                                       in (checkpoint' pt sf'', b)

forgetPast sf = SF $ \a -> let (sf', b) = sfTF sf a
                           in (forgetPast' 0 sf', b)

forgetPast' time sf' = SF' $ \dt a -> let time' = time + dt
                                      in -- trace (show time') $
                                          if time' < 0
                                           then let (sf'', b) = sfTF' sf' (-time) a
                                                in (forgetPast' 0 sf'', b)
                                           else let (sf'', b) = sfTF' sf' dt a
                                                in (forgetPast' time' sf'', b)

alwaysForward :: SF a b -> SF a b
alwaysForward sf = SF $ \a -> let (sf', b) = sfTF sf a
                              in (alwaysForward' sf', b)

alwaysForward' :: SF' a b -> SF' a b
alwaysForward' sf = SF' $ \dt a -> let (sf', b) = sfTF' sf (max dt (-dt)) a
                                   in (alwaysForward' sf', b)

limitHistory :: DTime -> SF a b -> SF a b
limitHistory time sf = SF $ \a -> let (sf', b) = sfTF sf a
                                  in (limitHistory' 0 time sf', b)

limitHistory' :: Time -> DTime -> SF' a b -> SF' a b
limitHistory' curT maxT sf' = SF' $ \dt a -> let curT' = curT + dt
                                                 time' = if curT' > maxT then maxT else curT'
                                             in -- trace (show (dt, curT, maxT, maxMaxT)) $
                                                 if time' < 0
                                                  then let (sf'', b) = sfTF' sf' (-curT) a
                                                       in (limitHistory' 0 maxT sf'', b)
                                                  else let (sf'', b) = sfTF' sf' dt a
                                                       in (limitHistory' time' maxT sf'', b)

clocked :: SF a DTime -> SF a b -> SF a b
clocked clockSF sf = SF $ \a -> let (sf', b)  = sfTF sf a
                                    (cSF', _) = sfTF clockSF a
                                in (clocked' cSF' sf', b)

clocked' :: SF' a DTime -> SF' a b -> SF' a b
clocked' clockSF sf = SF' $ \dt a -> let (cSF', dt') = sfTF' clockSF dt a
                                         (sf', b) = sfTF' sf dt' a
                                     in (clocked' cSF' sf', b)

-- -- restartRevOn :: SF a b -> SF a (Event c) -> SF a b
-- -- restartRevOn sf sfc = switch (sf &&& sfc)
-- --                              (\_ -> restartOn sf sfc)
-- --
--
-- -- * Time access and time manipulation.
--
--
-- timeTransform :: (DTime -> DTime) -> SF a b -> SF a b
-- timeTransform transform sf = SF tf
--  where tf a = let (sf', b) = (sfTF sf) a
--                   sf''     = timeTransformF transform sf'
--               in (sf'', b)
--
-- timeTransformF :: (DTime -> DTime) -> SF' a b -> SF' a b
-- timeTransformF transform sf = SF' tf
--  where tf dt a = let dt'      = transform dt
--                      (sf', b) = (sfTF' sf) dt' a
--                      sf''     = timeTransformF transform sf'
--                  in (sf'', b)
--
-- timeTransformSF :: SF a (DTime -> DTime) -> SF a b -> SF a b
-- timeTransformSF sfTime sf = SF tf
--  where tf a = let (sf', b) = (sfTF sf) a
--                   (sfTime',_) = (sfTF sfTime) a
--                   sf''     = timeTransformSF' sfTime' sf'
--               in (sf'', b)
--
--
-- timeTransformSF' :: SF' a (DTime -> DTime) -> SF' a b -> SF' a b
-- timeTransformSF' sfTime sf = SF' tf
--  where tf dt a = let (sfTime', transform) = (sfTF' sfTime) dt a
--                      dt'      = transform dt
--                      (sf', b) = (sfTF' sf) dt' a
--                      sf''     = timeTransformSF' sfTime' sf'
--                  in (sf'', b)
--
-- -- ** Time-reversible variants.
--
-- revSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
-- revSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
--     where
--         tf0 a0 =
--             case tf10 a0 of
--                 (sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
--                 (sf1, (_,  Event c0)) -> switchingPoint sf1 k (sfTF (k c0) a0)
--
--         switchingPoint :: SF' a (b, Event c) -> (c -> SF a b) -> (SF' a b, b) -> (SF' a b, b)
--         switchingPoint sf1 k (sfN', b) = (sf', b)
--           where sf' = SF' tf'
--                 tf' dt a = if | dt < 0  -> sfTF' (switchAux sf1 k) dt a
--                                            -- let (sf1', b') = sfTF' sf1 dt a
--                                            -- in (switchAux sf1' k, b')
--                               | dt > 0  -> switchingPoint' sf1 k dt (sfTF' sfN' dt a)
--                               | dt == 0 -> switchingPoint sf1 k (sfN', b)
--
--         switchingPoint' :: SF' a (b, Event c) -> (c -> SF a b) -> DTime -> (SF' a b, b) -> (SF' a b, b)
--         switchingPoint' sf1 k accumDT (sfN', b) = (sf', b)
--           where sf' = SF' tf'
--                 tf' dt a = let dt' = dt + accumDT
--                            in if | dt < 0  -> if | dt' < 0  -> sfTF' (switchAux sf1 k) dt' a
--                                                  | dt' > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
--                                                  | dt' == 0 -> switchingPoint' sf1 k accumDT (sfN', b)
--                                  | dt > 0  -> dt' `seq` switchingPoint' sf1 k dt' (sfTF' sfN' dt a)
--                                  | dt == 0 -> switchingPoint' sf1 k accumDT (sfN', b)
--
--
--         switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
--         switchAux sf1                          k = SF' tf
--             where
--                 tf dt a =
--                     case (sfTF' sf1) dt a of
--                         (sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
--                         (_,    (_, Event c)) -> switchingPoint sf1 k (sfTF (k c) a)
--
-- alwaysForward :: SF a b -> SF a b
-- alwaysForward sf = SF $ \a -> let (sf', b) = sfTF sf a
--                               in (alwaysForward' sf', b)
--
-- alwaysForward' :: SF' a b -> SF' a b
-- alwaysForward' sf = SF' $ \dt a -> let (sf', b) = sfTF' sf (max dt (-dt)) a
--                                    in (alwaysForward' sf', b)
--
-- checkpoint :: SF a (b, Event (), Event ()) -> SF a b
-- checkpoint sf = SF $ \a -> let (sf', (b, save, reset)) = sfTF sf a
--                            in case reset of
--                                 Event () -> error "loop"
--                                 NoEvent -> let pt = case save of
--                                                       Event () -> Just (Right sf)
--                                                       NoEvent  -> Nothing
--                                            in (checkpoint' pt sf', b)
--
-- checkpoint' :: Maybe (Either (SF' a (b, Event (), Event ())) (SF a (b, Event (), Event ())))
--             -> (SF' a (b, Event (), Event ()))
--             -> SF' a b
-- checkpoint' rstPt sf' = SF' $ \dt a -> let (sf'', (b, save, reset)) = sfTF' sf' dt a
--                                        in case reset of
--                                             Event () -> case rstPt of
--                                                           Nothing    ->  let pt = case save of
--                                                                                     Event () -> Just (Left sf'')
--                                                                                     NoEvent -> rstPt
--                                                                          in pt `seq` (checkpoint' pt sf'', b)
--
--                                                           Just (Left sf''') -> (checkpoint' rstPt sf''', b)
--                                                           Just (Right sf  ) -> sfTF (checkpoint sf) a
--                                             NoEvent -> let pt = case save of
--                                                                   Event () -> Just (Left sf'')
--                                                                   NoEvent -> rstPt
--                                                        in pt `seq` (checkpoint' pt sf'', b)
--
-- forgetPast sf = SF $ \a -> let (sf', b) = sfTF sf a
--                            in (forgetPast' 0 sf', b)
--
-- forgetPast' time sf' = SF' $ \dt a -> let time' = time + dt
--                                       in -- trace (show time') $
--                                           if time' < 0
--                                            then let (sf'', b) = sfTF' sf' (-time) a
--                                                 in (forgetPast' 0 sf'', b)
--                                            else let (sf'', b) = sfTF' sf' dt a
--                                                 in (forgetPast' time' sf'', b)
--
-- limitHistory :: DTime -> SF a b -> SF a b
-- limitHistory time sf = SF $ \a -> let (sf', b) = sfTF sf a
--                                   in (limitHistory' 0 time sf', b)
--
-- limitHistory' :: Time -> DTime -> SF' a b -> SF' a b
-- limitHistory' curT maxT sf' = SF' $ \dt a -> let curT' = curT + dt
--                                                  time' = if curT' > maxT then maxT else curT'
--                                              in -- trace (show (dt, curT, maxT, maxMaxT)) $
--                                                  if time' < 0
--                                                   then let (sf'', b) = sfTF' sf' (-curT) a
--                                                        in (limitHistory' 0 maxT sf'', b)
--                                                   else let (sf'', b) = sfTF' sf' dt a
--                                                        in (limitHistory' time' maxT sf'', b)
--
-- clocked :: SF a DTime -> SF a b -> SF a b
-- clocked clockSF sf = SF $ \a -> let (sf', b)  = sfTF sf a
--                                     (cSF', _) = sfTF clockSF a
--                                 in (clocked' cSF' sf', b)
--
-- clocked' :: SF' a DTime -> SF' a b -> SF' a b
-- clocked' clockSF sf = SF' $ \dt a -> let (cSF', dt') = sfTF' clockSF dt a
--                                          (sf', b) = sfTF' sf dt' a
--                                      in (clocked' cSF' sf', b)
