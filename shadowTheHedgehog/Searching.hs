{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    GeneralisedNewtypeDeriving, FlexibleContexts #-}

module Searching where

import Data.List ( find, findIndex )
import Data.Functor.Contravariant

class (Foldable t, Monad m, Contravariant s) => SearchMode s t a m b | s -> t, s -> m where
    search :: s a -> t a -> m b

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

