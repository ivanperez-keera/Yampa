module FRP.Yampa.Testing where

import FRP.Yampa.InternalCore

-- Testing bit
newtype FutureSF a b = FutureSF { unsafeSF :: SF' a b }

prefuturize :: SF a b -> a -> DTime -> (SF a b, b)
prefuturize (SF sf) a dt = let (sf', b) = sf a
  in (SF (sfTF' sf' dt), b)

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

type FutureSampleStream a = [(DTime, a)]
type SignalSampleStream a = (a, FutureSampleStream a)

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
