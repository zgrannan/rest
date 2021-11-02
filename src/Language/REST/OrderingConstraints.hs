{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.REST.OrderingConstraints
  (
    OrderingConstraints(..)
  , ConstraintGen
  , liftC
  , cmapConstraints
  , numOrderings
  , isUnsatisfiable
  , intersectAll
  , unionAll
  , intersectRelation
  , runStateConstraints
  , singleton
  )  where

import Control.Monad.Identity
import Control.Monad.State.Strict
import qualified Data.List as L
import Data.Hashable
import Debug.Trace
import qualified Data.Set as S

import Prelude hiding (GT, EQ)

import qualified Language.REST.WQO as WQO
import Language.REST.Types
import Language.REST.SMT (ToSMTVar)

type WQO = WQO.WQO

trace' _ x = x

data OrderingConstraints impl m = OC
  { addConstraint       :: forall a. (Eq a, Ord a, Hashable a) => WQO a -> impl a -> impl a
  , intersect           :: forall a. (Show a, Eq a, Ord a, Hashable a) => impl a -> impl a -> impl a
  , isSatisfiable       :: forall a. (ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => impl a -> m Bool
  , notStrongerThan     :: forall a. (ToSMTVar a Int, Eq a, Ord a, Hashable a) => impl a -> impl a -> m Bool
  , noConstraints       :: forall a. (Eq a, Ord a, Hashable a) => impl a
  , permits             :: forall a. (Show a, Eq a, Ord a, Hashable a) => impl a -> WQO a -> Bool
  , relevantConstraints :: forall a. (Eq a, Ord a, Hashable a) => impl a -> S.Set a -> S.Set a -> impl a
  , union               :: forall a. (Eq a, Ord a, Hashable a) => impl a -> impl a -> impl a
  , unsatisfiable       :: forall a. impl a
  , elems               :: forall a. (Eq a, Ord a, Hashable a) => impl a -> S.Set a
  , getOrdering         :: forall a. impl a -> Maybe (WQO a)
  , simplify            :: forall a. (Eq a, Ord a, Hashable a) => impl a -> impl a
  }

numOrderings :: (Show a, Ord a, Eq a, Ord a, Hashable a) => S.Set a -> OrderingConstraints oc m -> oc a -> Int
numOrderings elems impl oc = S.size $ S.filter (permits impl oc) (WQO.orderings elems)

isUnsatisfiable :: (Functor m, ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => OrderingConstraints oc m -> oc a -> m Bool
isUnsatisfiable OC{isSatisfiable} c = not <$> isSatisfiable c

singleton :: (Eq a, Ord a, Hashable a) => OrderingConstraints oc m -> WQO a -> oc a
singleton OC{addConstraint, noConstraints} c = addConstraint c noConstraints

intersectAll :: (Eq a, Ord a, Hashable a, Show a, Show (oc a)) => OrderingConstraints oc m -> [oc a] -> oc a
intersectAll OC{noConstraints} []     = noConstraints
intersectAll OC{intersect}     (x:xs) = L.foldl' go x xs
  where
    go t1 t2 = trace' ("Intersect " ++ (show t1)) $ intersect t1 t2

unionAll :: (Eq a, Ord a, Hashable a, Show a, Show (oc a)) => OrderingConstraints oc m -> [oc a] -> oc a
unionAll OC{unsatisfiable} []     = unsatisfiable
unionAll OC{union}         (x:xs) = L.foldl' go x xs
  where
    go t1 t2 = trace' ("Union " ++ (show t1)) $ union t1 t2

intersectRelation ::
  (Ord a, Eq a, Ord a, Hashable a, Show a) =>
  OrderingConstraints oc m -> oc a -> (a, a, Relation) -> oc a
intersectRelation oc impl (f, g, r) =
  case nc r of
    Just impl' -> intersect oc impl impl'
    Nothing    -> unsatisfiable oc
  where
    nc GT  = fmap (singleton oc) (WQO.singleton (f, g, WQO.QGT))
    nc EQ  = fmap (singleton oc) (WQO.singleton (f, g, WQO.QEQ))
    nc GTE = do
      wqo1 <- WQO.singleton (f, g, WQO.QGT)
      wqo2 <- WQO.singleton (f, g, WQO.QEQ)
      return $ union oc (singleton oc wqo1) (singleton oc wqo2)



-- ConstraintGen impl R >= t u returns the constraints on >= that guarantee
-- the resulting relation >=', we have:
--   1. x >= y implies x >=' y
--   2. t lift(R(>=')) u
-- Where R generates { == , >=, > } from the underlying ordering
-- R is used to enable optimizations

type ConstraintGen oc base lifted m =
  forall m' . (OrderingConstraints oc m' -> Relation -> oc base -> lifted -> lifted -> m (oc base))

cmapConstraints :: (lifted' -> lifted) -> ConstraintGen oc base lifted m -> ConstraintGen oc base lifted' m
cmapConstraints f cgen impl r oc t u = cgen impl r oc (f t) (f u)

liftC :: (m Bool  -> m' Bool) -> OrderingConstraints impl m -> OrderingConstraints impl m'
liftC f oc = oc{
    isSatisfiable   = isSatisfiable'
  , notStrongerThan = notStrongerThan'
  }
  where
    isSatisfiable'   c1    = f (isSatisfiable oc c1)
    notStrongerThan' c1 c2 = f (notStrongerThan oc c1 c2)

runStateConstraints :: ConstraintGen oc base lifted (State a) -> a -> ConstraintGen oc base lifted Identity
runStateConstraints cgen initState impl r oc t u = Identity $ evalState (cgen impl r oc t u) initState
