{-# LANGUAGE Rank2Types #-}
-- |
-- Module      :  FRP.Yampa.Switches
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Switches allow you to change the signal function being applied.
--
-- The basic idea of switching is fromed by combining a subordinate signal
-- function and a signal function continuation parameterised over some initial
-- data.
--
-- For example, the most basic switch has the following signature:
--
-- @switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b@
--
-- which indicates that it has two parameters: a signal function
-- that produces an output and indicates, with an event, when it is time to
-- switch, and a signal function that starts with the residual data left by the
-- first SF in the event and continues onwards.
--
-- Switching occurs, at most, once. If you want something to switch repeatedly,
-- in general, you need to loop, or to switch onto the same signal function
-- again. However, some switches, explained below, are immediate (meaning that
-- the second SF is started at the time of switching). If you use the same SF
-- that originally provoked the switch, you are very likely to fall into an
-- infinite loop. In those cases, the use of 'dSwitch' or '-->' may help.
--
-- Switches vary depending on a number of criterions:
--
-- - /Decoupled/ vs normal switching /(d)/: when an SF is being applied and a
-- different SF needs to be applied next, one question is which one is used
-- for the time in which the switching takes place. In decoupled switching, the
-- old SF is used for the time of switching, and the one SF is only used after
-- that. In normal or instantaneous or coupled switching, the old SF is
-- discarded immediately and a new SF is used for the output already from that
-- point in time.
--
-- - How the switching event is provided /( \/r\/k)/: normally, an 'Event' is
-- used to indicate that a switching must take place. This event can be part of
-- the argument SF (e.g., 'switch'), it can be part of the input (e.g.,
-- 'rSwitch'), or it can be determined by a second argument SF (e.g,
-- 'kSwitch').
--
-- - How many SFs are being handled /( \/p\/par)/: some combinators deal with
-- only one SF, others handle collections, either in the form of a
--'Functor' or a list ('[]').
--
-- - How the input is router /(B\/Z\/ )/: when multiple SFs are being combined,
-- a decision needs to be made about how the input is passed to the internal
-- SFs.  In some cases, broadcasting is used to pass the same input to all
-- internal SFs. In others, the input is itself a collection, and each element
-- is passed to one internal SF (i.e., /zipping/). In others, an auxiliary
-- function is used to decide how to route specific inputs to specific SFs in
-- the collection.
--
-- These gives a number of different combinations, some of which make no sense,
-- and also helps determine the expected behaviour of a combinator by looking
-- at its name. For example, 'drpSwitchB' is the decoupled (/d/), recurrent
-- (/r/), parallel (/p/) switch with broadcasting (/B/).
module FRP.Yampa.Switches (
    -- * Basic switching
    switch,  dSwitch,   -- :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
    rSwitch, drSwitch,  -- :: SF a b -> SF (a,Event (SF a b)) b
    kSwitch, dkSwitch,  -- :: SF a b
                        --    -> SF (a,b) (Event c)
                        --    -> (SF a b -> c -> SF a b)
                        --    -> SF a b

    -- * Parallel composition\/switching (collections)
    -- ** With broadcasting
    parB,               -- :: Functor col => col (SF a b) -> SF a (col b)
    pSwitchB,dpSwitchB, -- :: Functor col =>
                        --        col (SF a b)
                        --        -> SF (a, col b) (Event c)
                        --        -> (col (SF a b) -> c -> SF a (col b))
                        --        -> SF a (col b)
    rpSwitchB,drpSwitchB,-- :: Functor col =>
                        --        col (SF a b)
                        --        -> SF (a, Event (col (SF a b)->col (SF a b)))
                        --              (col b)

    -- ** With helper routing function
    par,                -- Functor col =>
                        --     (forall sf . (a -> col sf -> col (b, sf)))
                        --     -> col (SF b c)
                        --     -> SF a (col c)
    pSwitch, dpSwitch,  -- pSwitch :: Functor col =>
                        --     (forall sf . (a -> col sf -> col (b, sf)))
                        --     -> col (SF b c)
                        --     -> SF (a, col c) (Event d)
                        --     -> (col (SF b c) -> d -> SF a (col c))
                        --     -> SF a (col c)
    rpSwitch,drpSwitch, -- Functor col =>
                        --    (forall sf . (a -> col sf -> col (b, sf)))
                        --    -> col (SF b c)
                        --    -> SF (a, Event (col (SF b c) -> col (SF b c)))
                        --          (col c)
                        --
    -- * Parallel composition\/switching (lists)
    --
    -- ** With "zip" routing
    parZ,         -- [SF a b] -> SF [a] [b]
    pSwitchZ,     -- [SF a b] -> SF ([a],[b]) (Event c)
                  -- -> ([SF a b] -> c -> SF [a] [b]) -> SF [a] [b]
    dpSwitchZ,    -- [SF a b] -> SF ([a],[b]) (Event c)
                  -- -> ([SF a b] -> c ->SF [a] [b]) -> SF [a] [b]
    rpSwitchZ,    -- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]
    drpSwitchZ,   -- [SF a b] -> SF ([a], Event ([SF a b]->[SF a b])) [b]

    -- ** With replication
    parC,         -- SF a b -> SF [a] [b]

) where

import Control.Arrow

import FRP.Yampa.Basic
import FRP.Yampa.Diagnostics
import FRP.Yampa.Event
import FRP.Yampa.InternalCore (DTime, FunDesc (..), SF (..), SF' (..), fdFun,
                               sfArrG, sfConst, sfTF')

------------------------------------------------------------------------------
-- Basic switches
------------------------------------------------------------------------------
-- | Basic switch.
--
-- By default, the first signal function is applied. Whenever the second value
-- in the pair actually is an event, the value carried by the event is used to
-- obtain a new signal function to be applied *at that time and at future
-- times*. Until that happens, the first value in the pair is produced in the
-- output signal.
--
-- Important note: at the time of switching, the second signal function is
-- applied immediately. If that second SF can also switch at time zero, then a
-- double (nested) switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and never be
-- resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            case tf10 a0 of
                (sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
                (_,   (_,  Event c0)) -> sfTF (k c0) a0

        -- It would be nice to optimize further here. E.g. if it would be
        -- possible to observe the event source only.
        switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
        switchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
        switchAux (SFArr _ fd1)                k = switchAuxA1 (fdFun fd1) k
        switchAux sf1                          k = SF' tf
            where
                tf dt a =
                    case (sfTF' sf1) dt a of
                        (sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
                        (_,    (_, Event c)) -> sfTF (k c) a

        -- Note: While switch behaves as a stateless arrow at this point, that
        -- could change after a switch. Hence, SF' overall.
        switchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
        switchAuxA1 f1 k = sf
            where
                sf     = SF' tf -- False
                tf _ a =
                    case f1 a of
                        (b, NoEvent) -> (sf, b)
                        (_, Event c) -> sfTF (k c) a


-- | Switch with delayed observation.
--
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event,
-- the value carried by the event is used to obtain a new signal
-- function to be applied *at future times*.
--
-- Until that happens, the first value in the pair is produced
-- in the output signal.
--
-- Important note: at the time of switching, the second
-- signal function is used immediately, but the current
-- input is fed by it (even though the actual output signal
-- value at time 0 is discarded).
--
-- If that second SF can also switch at time zero, then a
-- double (nested) -- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and never be
-- resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
dSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let (sf1, (b0, ec0)) = tf10 a0
            in (case ec0 of
                    NoEvent  -> dSwitchAux sf1 k
                    Event c0 -> fst (sfTF (k c0) a0),
                b0)

        -- It would be nice to optimize further here. E.g. if it would be
        -- possible to observe the event source only.
        dSwitchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
        dSwitchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
        dSwitchAux (SFArr _ fd1)                k = dSwitchAuxA1 (fdFun fd1) k
        dSwitchAux sf1                          k = SF' tf
            where
                tf dt a =
                    let (sf1', (b, ec)) = (sfTF' sf1) dt a
                    in (case ec of
                            NoEvent -> dSwitchAux sf1' k
                            Event c -> fst (sfTF (k c) a),

                        b)

        -- Note: While dSwitch behaves as a stateless arrow at this point, that
        -- could change after a switch. Hence, SF' overall.
        dSwitchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
        dSwitchAuxA1 f1 k = sf
            where
                sf = SF' tf -- False
                tf _ a =
                    let (b, ec) = f1 a
                    in (case ec of
                            NoEvent -> sf
                            Event c -> fst (sfTF (k c) a),

                        b)


-- | Recurring switch.
--
-- Uses the given SF until an event comes in the input, in which case the SF in
-- the event is turned on, until the next event comes in the input, and so on.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more
-- information on how this switch works.
rSwitch :: SF a b -> SF (a, Event (SF a b)) b
rSwitch sf = switch (first sf) ((noEventSnd >=-) . rSwitch)


-- | Recurring switch with delayed observation.
--
-- Uses the given SF until an event comes in the input, in which case the SF in
-- the event is turned on, until the next event comes in the input, and so on.
--
-- Uses decoupled switch ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more
-- information on how this switch works.
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)


-- | Call-with-current-continuation switch.
--
-- Applies the first SF until the input signal and the output signal, when
-- passed to the second SF, produce an event, in which case the original SF and
-- the event are used to build an new SF to switch into.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more
-- information on how this switch works.
kSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let (sf1, b0) = tf10 a0
            in
                case tfe0 (a0, b0) of
                    (sfe, NoEvent)  -> (kSwitchAux sf1 sfe, b0)
                    (_,   Event c0) -> sfTF (k sf10 c0) a0

        kSwitchAux (SFArr _ (FDC b)) sfe = kSwitchAuxC1 b sfe
        kSwitchAux (SFArr _ fd1)     sfe = kSwitchAuxA1 (fdFun fd1) sfe
        kSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        kSwitchAux sf1 (SFArr _ fde) = kSwitchAuxAE sf1 (fdFun fde)
        kSwitchAux sf1            sfe                 = SF' tf -- False
            where
                tf dt a =
                    let (sf1', b) = (sfTF' sf1) dt a
                    in
                        case (sfTF' sfe) dt (a, b) of
                            (sfe', NoEvent) -> (kSwitchAux sf1' sfe', b)
                            (_,    Event c) -> sfTF (k (freeze sf1 dt) c) a

        -- !!! Untested optimization!
        kSwitchAuxC1 b (SFArr _ (FDC NoEvent)) = sfConst b
        kSwitchAuxC1 b (SFArr _ fde)        = kSwitchAuxC1AE b (fdFun fde)
        kSwitchAuxC1 b sfe                 = SF' tf -- False
            where
                tf dt a =
                    case (sfTF' sfe) dt (a, b) of
                        (sfe', NoEvent) -> (kSwitchAuxC1 b sfe', b)
                        (_,    Event c) -> sfTF (k (constant b) c) a

        -- !!! Untested optimization!
        kSwitchAuxA1 f1 (SFArr _ (FDC NoEvent)) = sfArrG f1
        kSwitchAuxA1 f1 (SFArr _ fde)        = kSwitchAuxA1AE f1 (fdFun fde)
        kSwitchAuxA1 f1 sfe                 = SF' tf -- False
            where
                tf dt a =
                    let b = f1 a
                    in
                        case (sfTF' sfe) dt (a, b) of
                            (sfe', NoEvent) -> (kSwitchAuxA1 f1 sfe', b)
                            (_,    Event c) -> sfTF (k (arr f1) c) a

        -- !!! Untested optimization!
        kSwitchAuxAE (SFArr _ (FDC b))  fe = kSwitchAuxC1AE b fe
        kSwitchAuxAE (SFArr _ fd1)   fe = kSwitchAuxA1AE (fdFun fd1) fe
        kSwitchAuxAE sf1            fe = SF' tf -- False
            where
                tf dt a =
                    let (sf1', b) = (sfTF' sf1) dt a
                    in
                        case fe (a, b) of
                            NoEvent -> (kSwitchAuxAE sf1' fe, b)
                            Event c -> sfTF (k (freeze sf1 dt) c) a

        -- !!! Untested optimization!
        kSwitchAuxC1AE b fe = SF' tf -- False
            where
                tf _ a =
                    case fe (a, b) of
                        NoEvent -> (kSwitchAuxC1AE b fe, b)
                        Event c -> sfTF (k (constant b) c) a

        -- !!! Untested optimization!
        kSwitchAuxA1AE f1 fe = SF' tf -- False
            where
                tf _ a =
                    let b = f1 a
                    in
                        case fe (a, b) of
                            NoEvent -> (kSwitchAuxA1AE f1 fe, b)
                            Event c -> sfTF (k (arr f1) c) a


-- | 'kSwitch' with delayed observation.
--
-- Applies the first SF until the input signal and the output signal, when
-- passed to the second SF, produce an event, in which case the original SF and
-- the event are used to build an new SF to switch into.
--
-- The switch is decoupled ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more
-- information on how this switch works.
dkSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
dkSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let (sf1, b0) = tf10 a0
            in (case tfe0 (a0, b0) of
                    (sfe, NoEvent)  -> dkSwitchAux sf1 sfe
                    (_,   Event c0) -> fst (sfTF (k sf10 c0) a0),
                b0)

        dkSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        dkSwitchAux sf1 sfe                     = SF' tf -- False
            where
                tf dt a =
                    let (sf1', b) = (sfTF' sf1) dt a
                    in (case (sfTF' sfe) dt (a, b) of
                            (sfe', NoEvent) -> dkSwitchAux sf1' sfe'
                            (_, Event c) -> fst (sfTF (k (freeze sf1 dt) c) a),
                        b)


------------------------------------------------------------------------------
-- Parallel composition and switching over collections with broadcasting
------------------------------------------------------------------------------

-- | Tuple a value up with every element of a collection of signal
-- functions.
broadcast :: Functor col => a -> col sf -> col (a, sf)
broadcast a = fmap (\sf -> (a, sf))


-- | Spatial parallel composition of a signal function collection.
-- Given a collection of signal functions, it returns a signal
-- function that broadcasts its input signal to every element
-- of the collection, to return a signal carrying a collection
-- of outputs. See 'par'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
parB :: Functor col => col (SF a b) -> SF a (col b)
parB = par broadcast

-- | Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel) with broadcasting. See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
pSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c-> SF a (col b))
    -> SF a (col b)
pSwitchB = pSwitch broadcast

-- | Decoupled parallel switch with broadcasting (dynamic collection of
--   signal functions spatially composed in parallel). See 'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
dpSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c->SF a (col b))
    -> SF a (col b)
dpSwitchB = dpSwitch broadcast

-- | Recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the input, in
-- which case the function in the 'Event' is used to transform the collections
-- of SF to be used with 'rpSwitch' again, until the next event comes in the
-- input, and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to each SF in
-- the collection.
--
-- See 'rpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
rpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchB = rpSwitch broadcast

-- | Decoupled recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the input, in
-- which case the function in the 'Event' is used to transform the collections
-- of SF to be used with 'rpSwitch' again, until the next event comes in the
-- input, and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to each SF in
-- the collection.
--
-- This is the decoupled version of 'rpSwitchB'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
drpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchB = drpSwitch broadcast

------------------------------------------------------------------------------
-- Parallel composition and switching over collections with general routing
------------------------------------------------------------------------------

-- | Spatial parallel composition of a signal function collection parameterized
-- on the routing function.
--
par :: Functor col
    => (forall sf . (a -> col sf -> col (b, sf)))
         -- ^ Determines the input to each signal function
         --     in the collection. IMPORTANT! The routing function MUST
         --     preserve the structure of the signal function collection.
    -> col (SF b c)
         -- ^ Signal function collection.
    -> SF a (col c)
par rf sfs0 = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                sfs   = fmap fst sfcs0
                cs0   = fmap snd sfcs0
            in
                (parAux rf sfs, cs0)


-- Internal definition. Also used in parallel switchers.
parAux :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF' b c)
    -> SF' a (col c)
parAux rf sfs = SF' tf -- True
    where
        tf dt a =
            let bsfs  = rf a sfs
                sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                sfs'  = fmap fst sfcs'
                cs    = fmap snd sfcs'
            in
                (parAux rf sfs', cs)


-- | Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle
-- can be derived. The signal function collection is spatially composed in
-- parallel and run until the event signal function has an occurrence. Once
-- the switching event occurs, all signal function are "frozen" and their
-- continuations are passed to the continuation function, along with the
-- event value.
pSwitch :: Functor col
        => (forall sf . (a -> col sf -> col (b, sf)))
              -- ^ Routing function: determines the input to each signal
              -- function in the collection. IMPORTANT! The routing function
              -- has an obligation to preserve the structure of the signal
              -- function collection.
        -> col (SF b c)
              -- ^ Signal function collection.
        -> SF (a, col c) (Event d)
              -- ^ Signal function generating the switching event.
        -> (col (SF b c) -> d -> SF a (col c))
              -- ^ Continuation to be invoked once event occurs.
        -> SF a (col c)
pSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                sfs   = fmap fst sfcs0
                cs0   = fmap snd sfcs0
            in
                case (sfTF sfe0) (a0, cs0) of
                    (sfe, NoEvent)  -> (pSwitchAux sfs sfe, cs0)
                    (_,   Event d0) -> sfTF (k sfs0 d0) a0

        pSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
        pSwitchAux sfs sfe = SF' tf -- False
            where
                tf dt a =
                    let bsfs  = rf a sfs
                        sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                        sfs'  = fmap fst sfcs'
                        cs    = fmap snd sfcs'
                    in
                        case (sfTF' sfe) dt (a, cs) of
                            (sfe', NoEvent) -> (pSwitchAux sfs' sfe', cs)
                            (_,    Event d) -> sfTF (k (freezeCol sfs dt) d) a


-- | Parallel switch with delayed observation parameterized on the routing
-- function.
--
-- The collection argument to the function invoked on the
-- switching event is of particular interest: it captures the
-- continuations of the signal functions running in the collection
-- maintained by 'dpSwitch' at the time of the switching event,
-- thus making it possible to preserve their state across a switch.
-- Since the continuations are plain, ordinary signal functions,
-- they can be resumed, discarded, stored, or combined with
-- other signal functions.
dpSwitch :: Functor col
         => (forall sf . (a -> col sf -> col (b, sf)))
              -- ^ Routing function. Its purpose is to pair up each running
              -- signal function in the collection maintained by 'dpSwitch'
              -- with the input it is going to see at each point in time. All
              -- the routing function can do is specify how the input is
              -- distributed.
         -> col (SF b c)
              -- ^ Initial collection of signal functions.
         -> SF (a, col c) (Event d)
              -- ^ Signal function that observes the external input signal and
              -- the output signals from the collection in order to produce a
              -- switching event.
         -> (col (SF b c) -> d -> SF a (col c))
              -- ^ The fourth argument is a function that is invoked when the
              -- switching event occurs, yielding a new signal function to
              -- switch into based on the collection of signal functions
              -- previously running and the value carried by the switching
              -- event. This allows the collection to be updated and then
              -- switched back in, typically by employing 'dpSwitch' again.
         -> SF a (col c)
dpSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
        tf0 a0 =
            let bsfs0 = rf a0 sfs0
                sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
                cs0   = fmap snd sfcs0
            in
                (case (sfTF sfe0) (a0, cs0) of
                     (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
                     (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0),
                 cs0)

        dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
        dpSwitchAux sfs sfe = SF' tf -- False
            where
                tf dt a =
                    let bsfs  = rf a sfs
                        sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
                        cs    = fmap snd sfcs'
                    in
                        (case (sfTF' sfe) dt (a, cs) of
                             (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs')
                                                            sfe'
                             (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt)
                                                             d)
                                                          a),
                         cs)


-- | Recurring parallel switch parameterized on the routing function.
--
-- Uses the given collection of SFs, until an event comes in the input, in
-- which case the function in the 'Event' is used to transform the collections
-- of SF to be used with 'rpSwitch' again, until the next event comes in the
-- input, and so on.
--
-- The routing function is used to decide which subpart of the input
-- goes to each SF in the collection.
--
-- This is the parallel version of 'rSwitch'.
rpSwitch :: Functor col
         => (forall sf . (a -> col sf -> col (b, sf)))
               -- ^ Routing function: determines the input to each signal
               -- function in the collection. IMPORTANT! The routing function
               -- has an obligation to preserve the structure of the signal
               -- function collection.
         -> col (SF b c)
               -- ^ Initial signal function collection.
         -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
rpSwitch rf sfs =
    pSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- rpSwitch rf (f sfs')


-- | Recurring parallel switch with delayed observation parameterized on the
-- routing function.
--
-- Uses the given collection of SFs, until an event comes in the input, in
-- which case the function in the 'Event' is used to transform the collections
-- of SF to be used with 'rpSwitch' again, until the next event comes in the
-- input, and so on.
--
-- The routing function is used to decide which subpart of the input
-- goes to each SF in the collection.
--
-- This is the parallel version of 'drSwitch'.
drpSwitch :: Functor col
          => (forall sf . (a -> col sf -> col (b, sf)))
                -- ^ Routing function: determines the input to each signal
                -- function in the collection. IMPORTANT! The routing function
                -- has an obligation to preserve the structure of the signal
                -- function collection.
          -> col (SF b c)
                -- ^ Initial signal function collection.
          -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
drpSwitch rf sfs =
    dpSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- drpSwitch rf (f sfs')

------------------------------------------------------------------------------
-- * Parallel composition/switchers with "zip" routing
------------------------------------------------------------------------------

-- | Parallel composition of a list of SFs.
--
--   Given a list of SFs, returns an SF that takes a list of inputs, applies
--   each SF to each input in order, and returns the SFs' outputs.
--
--   >>> embed (parZ [arr (+1), arr (+2)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
--   [[1,2],[2,3]]
--
--   If there are more SFs than inputs, an exception is thrown.
--
--   >>> embed (parZ [arr (+1), arr (+1), arr (+2)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
--   [[1,1,*** Exception: FRP.Yampa.Switches.parZ: Input list too short.
--
--   If there are more inputs than SFs, the unused inputs are ignored.
--
--   >>> embed (parZ [arr (+1)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
--   [[1],[2]]

parZ :: [SF a b] -> SF [a] [b]
parZ = par (safeZip "parZ")

-- | Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel). See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
pSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c -> SF [a] [b])
            -> SF [a] [b]
pSwitchZ = pSwitch (safeZip "pSwitchZ")

-- | Decoupled parallel switch with broadcasting (dynamic collection of
--   signal functions spatially composed in parallel). See 'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
dpSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c ->SF [a] [b])
             -> SF [a] [b]
dpSwitchZ = dpSwitch (safeZip "dpSwitchZ")

-- | Recurring parallel switch with "zip" routing.
--
-- Uses the given list of SFs, until an event comes in the input, in which case
-- the function in the 'Event' is used to transform the list of SF to be used
-- with 'rpSwitchZ' again, until the next event comes in the input, and so on.
--
-- Zip routing is used to decide which subpart of the input goes to each SF in
-- the list.
--
-- See 'rpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
rpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
rpSwitchZ = rpSwitch (safeZip "rpSwitchZ")

-- | Decoupled recurring parallel switch with "zip" routing.
--
-- Uses the given list of SFs, until an event comes in the input, in which case
-- the function in the 'Event' is used to transform the list of SF to be used
-- with 'rpSwitchZ' again, until the next event comes in the input, and so on.
--
-- Zip routing is used to decide which subpart of the input goes to each SF in
-- the list.
--
-- See 'rpSwitchZ' and 'drpSwitch'.
--
-- For more information on how parallel composition works, check
-- <http://haskell.cs.yale.edu/wp-content/uploads/2011/01/yampa-arcade.pdf>
drpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
drpSwitchZ = drpSwitch (safeZip "drpSwitchZ")

safeZip :: String -> [a] -> [b] -> [(a,b)]
safeZip fn l1 l2 = safeZip' l1 l2
  where
    safeZip' :: [a] -> [b] -> [(a, b)]
    safeZip' _  []     = []
    safeZip' as (b:bs) = (head' as, b) : safeZip' (tail' as) bs

    head' :: [a] -> a
    head' []    = err
    head' (a:_) = a

    tail' :: [a] -> [a]
    tail' []     = err
    tail' (_:as) = as

    err :: a
    err = usrErr "FRP.Yampa.Switches" fn "Input list too short."


-- Freezes a "running" signal function, i.e., turns it into a continuation in
-- the form of a plain signal function.
freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs

-- | Apply an SF to every element of a list.
--
--   Example:
--
--   >>> embed (parC integral) (deltaEncode 0.1 [[1, 2], [2, 4], [3, 6], [4.0, 8.0 :: Float]])
--   [[0.0,0.0],[0.1,0.2],[0.3,0.6],[0.6,1.2]]
--
--   The number of SFs or expected inputs is determined by the first input
--   list, and not expected to vary over time.
--
--   If more inputs come in a subsequent list, they are ignored.
--
--   >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
--   [[1],[2],[4],[7],[2],[1],[2]]
--
--   If less inputs come in a subsequent list, an exception is thrown.
--
--   >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0, 0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
--   [[1,1],[2,2],[4,5],[7,8],[2,2],[1,1],[2,10]]

parC :: SF a b -> SF [a] [b]
parC sf = SF $ \as -> let os  = map (sfTF sf) as
                          bs  = map snd os
                          sfs = map fst os
                      in (parCAux sfs, bs)

-- Internal definition. Also used in parallel switchers.
parCAux :: [SF' a b] -> SF' [a] [b]
parCAux sfs = SF' tf
    where
        tf dt as =
            let os    = map (\(a,sf) -> sfTF' sf dt a) $ safeZip "parC" as sfs
                bs    = map snd os
                sfcs  = map fst os
            in
                (listSeq sfcs `seq` parCAux sfcs, listSeq bs)

listSeq :: [a] -> [a]
listSeq x = x `seq` (listSeq' x)

listSeq' :: [a] -> [a]
listSeq' []        = []
listSeq' rs@(a:as) = a `seq` listSeq' as `seq` rs

-- Vim modeline
-- vim:set tabstop=8 expandtab:
