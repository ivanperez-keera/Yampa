-- |
-- Copyright   :  (c) Ivan Perez, 2014-2022
--                (c) George Giorgidze, 2007-2012
--                (c) Henrik Nilsson, 2005-2006
--                (c) Henrik Nilsson, Yale University, 2003-2004
-- License     :  BSD-style (see the LICENSE file in the distribution)
-- Maintainer  :  ivan.perez@keera.co.uk
--
-- Association list with automatic key assignment and identity-preserving map
-- and filter operations.
module IdentityList
    ( ILKey
    , IL
    , emptyIL
    , insertIL_
    , insertIL
    , listToIL
    , keysIL
    , elemsIL
    , assocsIL
    , deleteIL
    , updateIL
    , updateILWith
    , mapIL
    , filterIL
    , mapFilterIL
    , lookupIL
    , findIL
    , mapFindIL
    , findAllIL
    , mapFindAllIL
    )
  where

import Data.List (find)

-- * Data type definitions

-- | Identity-list key type
type ILKey = Int

-- | Identity-list, abstract. Instance of functor.

-- Invariants:
-- * Sorted in descending key order. (We don't worry about
--   key wrap around).
-- * Keys are NOT reused
data IL a = IL { ilNextKey :: ILKey, ilAssocs :: [(ILKey, a)] }

-- * Class instances

instance Functor IL where
  fmap f (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = [ (i, f a) | (i, a) <- kas ]}

-- * Constructors

emptyIL :: IL a
emptyIL = IL {ilNextKey = 0, ilAssocs = []}

insertIL_ :: a -> IL a -> IL a
insertIL_ a il = snd (insertIL a il)

insertIL :: a -> IL a -> (ILKey, IL a)
insertIL a (IL {ilNextKey = k, ilAssocs = kas}) = (k, il') where
    il' = IL {ilNextKey = k + 1, ilAssocs = (k, a) : kas}

listToIL :: [a] -> IL a
listToIL as = IL { ilNextKey = length as
                 , ilAssocs = reverse (zip [0..] as) -- Maintain invariant!
                 }

-- * Additional selectors

assocsIL :: IL a -> [(ILKey, a)]
assocsIL = ilAssocs

keysIL :: IL a -> [ILKey]
keysIL = map fst . ilAssocs

elemsIL :: IL a -> [a]
elemsIL = map snd . ilAssocs

-- * Mutators

deleteIL :: ILKey -> IL a -> IL a
deleteIL k (IL {ilNextKey = nk, ilAssocs = kas}) =
    IL {ilNextKey = nk, ilAssocs = deleteHlp kas}
  where
    deleteHlp []                                   = []
    deleteHlp kakas@(ka@(k', _) : kas) | k > k'    = kakas
                                       | k == k'   = kas
                                       | otherwise = ka : deleteHlp kas

updateIL :: ILKey -> a -> IL a -> IL a
updateIL k v l = updateILWith k (const v) l

updateILWith :: ILKey -> (a -> a) -> IL a -> IL a
updateILWith k f l = mapIL g l
  where g (k',v') | k == k'   = f v'
                  | otherwise = v'

-- * Filter and map operations

-- These are "identity-preserving", i.e. the key associated with an element
-- in the result is the same as the key of the element from which the
-- result element was derived.

mapIL :: ((ILKey, a) -> b) -> IL a -> IL b
mapIL f (IL {ilNextKey = nk, ilAssocs = kas}) =
  IL {ilNextKey = nk, ilAssocs = [(k, f ka) | ka@(k,_) <- kas]}

filterIL :: ((ILKey, a) -> Bool) -> IL a -> IL a
filterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
  IL {ilNextKey = nk, ilAssocs = filter p kas}

mapFilterIL :: ((ILKey, a) -> Maybe b) -> IL a -> IL b
mapFilterIL p (IL {ilNextKey = nk, ilAssocs = kas}) =
  IL { ilNextKey = nk
     , ilAssocs = [(k, b) | ka@(k, _) <- kas, Just b <- [p ka]]
     }

-- * Lookup operations

lookupIL :: ILKey -> IL a -> Maybe a
lookupIL k il = lookup k (ilAssocs il)

findIL :: ((ILKey, a) -> Bool) -> IL a -> Maybe a
findIL p (IL {ilAssocs = kas}) = findHlp kas
  where
    findHlp []                = Nothing
    findHlp (ka@(_, a) : kas) = if p ka then Just a else findHlp kas

mapFindIL :: ((ILKey, a) -> Maybe b) -> IL a -> Maybe b
mapFindIL p (IL {ilAssocs = kas}) = mapFindHlp kas
  where
    mapFindHlp []         = Nothing
    mapFindHlp (ka : kas) = case p ka of
                              Nothing     -> mapFindHlp kas
                              jb@(Just _) -> jb

findAllIL :: ((ILKey, a) -> Bool) -> IL a -> [a]
findAllIL p (IL {ilAssocs = kas}) = [ a | ka@(_, a) <- kas, p ka ]

mapFindAllIL:: ((ILKey, a) -> Maybe b) -> IL a -> [b]
mapFindAllIL p (IL {ilAssocs = kas}) = [ b | ka <- kas, Just b <- [p ka] ]
