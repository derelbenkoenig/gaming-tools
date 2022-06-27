{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    GeneralisedNewtypeDeriving, FlexibleContexts, GADTs, InstanceSigs, 
    TypeApplications #-}

module Searching where

import Data.List ( find, findIndex, elemIndex )
import Data.Functor.Contravariant

class (Foldable t, Monad m, Contravariant s) => SearchMode s t a m b | s -> t, s -> m where
    search :: s a -> t a -> m b

fmapSearch f s = fmap f . search s

newtype FilterSearch a = FilterSearch (Predicate a)
    deriving Contravariant

mkFilterSearch p = FilterSearch $ Predicate p

instance SearchMode FilterSearch [] a [] a where
    search (FilterSearch (Predicate p)) = filter p

newtype FindSearch a = FindSearch (Predicate a)
    deriving Contravariant

mkFindSearch p = FindSearch $ Predicate p

instance SearchMode FindSearch [] a Maybe a where
    search (FindSearch (Predicate p)) = find p

newtype FindIndexSearch p = FindIndexSearch (Predicate p)
    deriving Contravariant

mkFindIndexSearch p = FindIndexSearch $ Predicate p

instance SearchMode FindIndexSearch [] a Maybe Int where
    search (FindIndexSearch (Predicate p)) = findIndex p

data ElemIndexSearch a where
    ElemIndexSearch :: Eq a => a -> ElemIndexSearch a

--  this declaration does not compile, the instance sig is not "more general" than the class sig
instance (a ~ b) => Contravariant ElemIndexSearch where
    contramap :: (a ~ b) => (a -> b) -> ElemIndexSearch b -> ElemIndexSearch a
    contramap f (ElemIndexSearch b) = ElemIndexSearch (f b)

instance SearchMode ElemIndexSearch [] a Maybe Int where
    search (ElemIndexSearch a) = elemIndex a


-------------------------------------
-- trying a different way entirely...
-------------------------------------

data SearchMode1 s t a m b where
    SearchMode1 :: (Foldable t, Monad m, Contravariant s) =>
        (s a -> t a -> m b)
        -> SearchMode1 s t a m b

instance Functor (SearchMode1 s t a m) where
    fmap f (SearchMode1 search) = SearchMode1 newSearch where
        newSearch s as = fmap f (search s as)
