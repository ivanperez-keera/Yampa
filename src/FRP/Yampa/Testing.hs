module FRP.Yampa.Testing where

import FRP.Yampa.InternalCore
import FRP.Yampa.Stream

-- * Running/initialized SF's
-- | A wrapper around initialized SF continuations.
newtype FutureSF a b = FutureSF { unsafeSF :: SF' a b }

-- | Given a signal function and time delta,
-- it moves the signal function into the future,
-- returning a new uninitialized SF and the
-- initial output.
prefuturize :: SF a b -> a -> DTime -> (SF a b, b)
prefuturize (SF sf) a dt = let (sf', b) = sf a
  in (SF (sfTF' sf' dt), b)

-- * Evaluation

-- ** One-step evaluation
evalAtZero :: SF a b
           -> a                  -- Input sample
           -> (b, FutureSF a b)  -- Output x Continuation
evalAtZero (SF { sfTF = tf }) a = (b, FutureSF tf' )
  where (tf', b) = tf a

evalAt :: FutureSF a b
       -> DTime -> a         -- Input sample
       -> (b, FutureSF a b)  -- Output x Continuation
evalAt (FutureSF { unsafeSF = tf }) dt a = (b, FutureSF tf')
  where (tf', b) = (sfTF' tf) dt a

-- ** Stream-based evaluation
evalSF :: SF a b
       -> SignalSampleStream a
       -> (SignalSampleStream b, FutureSF a b)
evalSF sf (a, as) = (outputStrm, fsf')
  where (b,  fsf)  = evalAtZero sf a
        (bs, fsf') = evalFutureSF fsf as
        outputStrm = (b, bs)

evalFutureSF :: FutureSF a b
             -> FutureSampleStream a
             -> (FutureSampleStream b, FutureSF a b)
evalFutureSF fsf [] = ([], fsf)
evalFutureSF fsf ((dt, a):as) = (outputStrm, fsf'')
  where (b, fsf')   = evalAt fsf dt a
        (bs, fsf'') = evalFutureSF fsf' as
        outputStrm  = (dt, b) : bs
