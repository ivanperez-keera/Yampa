{-# LANGUAGE GADTs  #-}
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
module TemporalLogic where

------------------------------------------------------------------------------
import FRP.Yampa as Yampa
import FRP.Yampa.Testing
import SampleStreams

-- * Temporal Logics based on SFs
type SPred a b = (SF a b, a -> b -> Bool)

data TPred a where
   Prop       :: SPred a b -> TPred a
   And        :: TPred a -> TPred a -> TPred a
   Or         :: TPred a -> TPred a -> TPred a
   Not        :: TPred a -> TPred a
   Implies    :: TPred a -> TPred a -> TPred a
   Always     :: TPred a -> TPred a
   Eventually :: TPred a -> TPred a
   Next       :: TPred a -> TPred a
   Until      :: TPred a -> TPred a -> TPred a

-- TL Evaluation
evalT :: TPred a -> SignalSampleStream a -> Bool
evalT (Prop (sf,p))   = \stream -> let b = fst $ fst $ evalSF sf stream
                                       a = fst stream
                                   in p a b 
evalT (And t1 t2)     = \stream -> evalT t1 stream && evalT t2 stream
evalT (Or  t1 t2)     = \stream -> evalT t1 stream || evalT t2 stream
evalT (Implies t1 t2) = \stream -> not (evalT t1 stream) || evalT t2 stream
evalT (Always  t1)    = \stream -> evalT t1 stream && evalT (Next (Always t1)) stream
evalT (Eventually t1) = \stream -> evalT t1 stream || evalT (Next (Eventually t1)) stream
evalT (Until t1 t2)   = \stream -> (evalT t1 stream && evalT (Next (Until t1 t2)) stream)
                                   || evalT t2 stream
evalT (Next t1)       = \stream -> case stream of
                                    (a,[]) -> True
                                    (a1,(dt, a2):as) -> evalT (tauApp t1 a1 dt) (a2, as)

-- Tau-application (transportation to the future)
tauApp :: TPred a -> a -> DTime -> TPred a
tauApp (Prop (sf,p)) sample dtime = Prop (sf', p)
  where sf' = fst (prefuturize sf sample dtime)
tauApp (And t1 t2) s dt         = And (tauApp t1 s dt) (tauApp t2 s dt)
tauApp (Or t1 t2) s dt          = Or (tauApp t1 s dt) (tauApp t2 s dt)
tauApp (Not t1) s dt            = Not (tauApp t1 s dt)
tauApp (Implies t1 t2) s dt     = Implies (tauApp t1 s dt) (tauApp t2 s dt)
tauApp (Always t1) s dt         = Always (tauApp t1 s dt)
tauApp (Eventually t1) s dt     = Eventually (tauApp t1 s dt)
tauApp (Next t1) s dt           = Next (tauApp t1 s dt)
tauApp (Until t1 t2) s dt       = Until (tauApp t1 s dt) (tauApp t2 s dt)

always :: (b -> Bool) -> SF a b -> TestSampleStream a -> Bool
always p sf inputs =
     all p $ samples $ fst $ evalSF sf (adaptTestStream inputs)

next :: (b -> Bool) -> FutureSF a b -> DTime -> a -> Bool
next p sf dt input = p $ fst $ evalAt sf dt input 

now :: (b -> Bool) -> SF a b -> a -> Bool
now p sf input = p $ fst $ evalAtZero sf input
