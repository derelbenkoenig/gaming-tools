{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    GeneralisedNewtypeDeriving, FlexibleContexts, GADTs, InstanceSigs, 
    TypeApplications, StandaloneKindSignatures #-}

module Searching where

import Data.List ( find, findIndex, elemIndex )
import Data.Profunctor
import Data.Functor.Contravariant

type Search :: (* -> *) -> (* -> *) -> * -> * -> *
data Search t m a b where
    Search :: (Functor t, Foldable t, Monad m) =>
        { runSearch :: (t a -> m b) } -> Search t m a b

instance Profunctor (Search t m) where
    lmap f (Search search) = Search newSearch where
        newSearch as = search (fmap f as)
    rmap f (Search search) = Search newSearch where
        newSearch as = fmap f $ search as

mkFilterSearch p = Search (filter p)
mkFindSearch :: (a -> Bool) -> Search [] Maybe a a
mkFindSearch p = Search (find p)
mkFindIndexSearch p = Search (findIndex p)
-- actually using elemIndex wouldn't let me make a contravariant
-- in fact, it still doesn't this way either.
-- You can take the e and make a contravariant, but the e itself isn't
mkElemIndexSearch e = mkFindIndexSearch ((==) e)

data SearchMode p t m a b where
    SearchMode :: Contravariant p => { mkSearch :: (p a -> Search t m a b) } -> SearchMode p t m a b

filterSearchMode = SearchMode (mkFilterSearch . getPredicate)
findSearchMode = SearchMode (mkFindSearch . getPredicate)
findIndexSearchMode = SearchMode (mkFindIndexSearch . getPredicate)

-- elemIndexSearchMode = SearchMode (mkFindIndexSearch . (==) . Identity)
