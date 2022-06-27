{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    GeneralisedNewtypeDeriving, FlexibleContexts, TemplateHaskell #-}

module Searching where

import Data.List ( find, findIndex )
import Data.Functor.Contravariant
import Language.Haskell.TH

class (Foldable t, Monad m, Contravariant s) => SearchMode s t a m b | s -> t, s -> m where
    search :: s a -> t a -> m b

fmapSearch f s = fmap f . search s

genListPredicateSearch :: Monad m => String -> ((a -> Bool) -> [a] -> m a) -> Q [Dec]
genListPredicateSearch name fun = sequence [ntD, helperFunD, instanceD] 
    where
        ntD = do 
            ntName <- mkName name
            return [d|
                newtype $(ntName) a = $(ntName) (Predicate a)
                    deriving Contravariant
                    |]
        helperFunD = do
            hfName <- mkName ("mk" ++ name)
            return [d|$(hfName) p = $(ntName) $ Predicate p|]
        instanceD = undefined

-- this was me trying to use the actual functions to make a newytpe decl
--         $ newtypeD
--             ntName              -- name
--             [PlainTv aName]     -- type args
--             Nothing             -- Maybe Kind
--             (NormalC            -- constructor
--                 ntName              -- constructor name
--    -- BangType wich= ((Bang unpackedness strictness), Type)
--                 [bangType
--                     (return $ Bang NoSourceUnpackedNess NoSourceStrictness)
--                     (varT aName)
--                 ]
--             [derivClause Nothing []]                  -- derive clauses
        

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

