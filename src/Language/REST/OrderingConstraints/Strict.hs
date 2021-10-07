{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.OrderingConstraints.Strict (
      strictOC
    , strictOC'
    , addConstraint
    , difference
    , getOrdering
    , intersect
    , isSatisfiable
    , isUnsatisfiable
    , noConstraints
    , notStrongerThan
    , permits
    , relevantConstraints
    , union
    , unsatisfiable
    , singleton
    , StrictOC
    , elems
    ) where

import Control.Monad.Identity
import Debug.Trace
import Text.Printf
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S

import qualified Language.REST.OrderingConstraints as OC
import qualified Language.REST.WQO as WQO

type WQO = WQO.WQO

-- Represents a set of constraints on a WQO on type `a`

-- The constraints are represented as a set ws of WQOs
-- The constraints permit any WQO w that is a valid extension of some (w' in wqos)


data StrictOC a = StrictOC (S.Set (WQO a))
  deriving (Eq, Ord, Generic, Hashable)

instance (Show a, Eq a, Ord a, Hashable a) => Show (StrictOC a) where
  show (StrictOC cs) | S.null cs             = "unsatisfiable"
  show (StrictOC cs) | S.member WQO.empty cs = "no constraints"
  show (StrictOC cs) = L.intercalate " ∨ \n" (map show (S.toList cs))
    -- where
      -- show' o@(OpOrdering s) = if S.size s > 1 then printf "(%s)" (show o) else show o

getOrdering :: StrictOC a -> Maybe (WQO a)
getOrdering (StrictOC o) =
  listToMaybe (S.toList o)

elems (StrictOC sets) = S.unions $ map WQO.elems (S.toList sets)

noConstraints :: forall a. (Eq a, Ord a, Hashable a) => StrictOC a
noConstraints = StrictOC (S.singleton (WQO.empty))

unsatisfiable = StrictOC S.empty

isUnsatisfiable :: Eq a => StrictOC a -> Bool
isUnsatisfiable c = c == unsatisfiable

isSatisfiable :: Eq a => StrictOC a -> Bool
isSatisfiable c = c /= unsatisfiable

notStrongerThan :: forall m a. (Monad m, Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> m Bool
notStrongerThan (StrictOC lhs) (StrictOC rhs) = return False

-- The difference of two constraints `a` and `b` is new constraints such that
-- intersect (diff a b) b = a
difference :: (Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
difference (StrictOC lhs) (StrictOC rhs) =
    StrictOC (S.difference lhs rhs)

-- The union  of two constraints `a` and `b` is new constraints that only
-- permits an ordering if permitted by either `a` or `b`
union :: (Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
union (StrictOC lhs) (StrictOC rhs) =
  fromSet $ S.union lhs rhs

fromSet :: (Eq a, Ord a, Hashable a) => S.Set (WQO a) -> StrictOC a
fromSet oc = -- StrictOC oc
  StrictOC $ go [] (L.sortOn (length . WQO.elems) $ S.toList oc)
  where
    go include []       = S.fromList include
    go include (x : xs) =
        if any (`WQO.notStrongerThan` x) (include ++ xs)
            then go include xs
            else go (x : include) xs


-- The intersection of two constraints `a` and `b` is new constraints that only
-- permits the orderings permitted by both `a` and `b`
intersect :: (Show a, Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
intersect (StrictOC lhs) (StrictOC rhs) = result
  -- trace (printf "%s intersect %s yields %s" (show lhs) (show rhs) (show result)) result
    where
      result = fromSet $ S.fromList $
        do
          lhs' <- S.toList lhs
          rhs' <- S.toList rhs
          maybeToList (WQO.merge lhs' rhs')

addConstraint :: (Eq a, Ord a, Hashable a) => WQO a -> StrictOC a -> StrictOC a
addConstraint c (StrictOC oc) = StrictOC $ S.fromList $ do
  c'  <-  S.toList oc
  maybeToList $ WQO.merge c c'

singleton :: (Eq a, Ord a, Hashable a) => WQO a -> StrictOC a
singleton c = addConstraint c noConstraints

relevantConstraints :: forall a. (Eq a, Ord a, Hashable a) => StrictOC a -> S.Set a -> S.Set a -> StrictOC a
relevantConstraints (StrictOC oc0) as bs = go (S.toList oc0) unsatisfiable
  where
    go :: [WQO a] -> StrictOC a -> StrictOC a
    go []          oc   = oc
    go (o : rest) exist =
      let
        o' = WQO.relevantTo o as bs
      in
        if WQO.null o'
        then noConstraints
        else go rest (union (singleton o) exist)

permits :: (Eq a, Ord a, Hashable a) => StrictOC a -> WQO a -> Bool
permits (StrictOC permitted) desired =
  any (`WQO.notStrongerThan` desired) (S.toList permitted)

strictOC :: Monad m => OC.OrderingConstraints StrictOC m
strictOC = OC.OC
  addConstraint
  intersect
  (return . isSatisfiable)
  notStrongerThan
  noConstraints
  permits
  relevantConstraints
  union
  unsatisfiable
  elems
  getOrdering
  id

strictOC' :: OC.OrderingConstraints StrictOC Identity
strictOC' = strictOC
