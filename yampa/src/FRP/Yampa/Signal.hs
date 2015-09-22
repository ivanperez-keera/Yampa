{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts       #-}

module FRP.Yampa.Signal where

import FRP.Yampa.InternalCore (DTime)

-- ** Sources, signals and Sinks
class Source a d m | a -> d, a -> m where
  initializeSo :: m a
  pollSo       :: a -> m d

class Sink a d m | a -> d, a -> m where
  initializeSi :: m a
  pushSi       :: a -> d -> m ()

class Signal a d m | a -> d, a -> m where
  initializeSg :: m (a, d)
  pollSg       :: a -> m (DTime, d)

-- Potential extensions
--
-- Lifting, applicative, functor, etc. for Signals, Sources, co-functor for
-- Sink.
--
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE FlexibleContexts       #-}
-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE UndecidableInstances   #-}
--
-- MySDLInput = SDLClock `Governing` NullInput
--
-- data Governing a b = Governing a b
-- 
-- instance (Monad m, Signal a c m, Source b d m) => Signal (Governing a b) d m where
--   initializeSg = do
--     (a,_) <- initializeSg
--     s     <- initializeSo
--     initialVal <- pollSo s
--     return (Governing a s, initialVal)
-- 
--   pollSg (Governing a b) = do
--     (dt,_) <- pollSg a
--     v      <- pollSo b
--     return (dt, v)
