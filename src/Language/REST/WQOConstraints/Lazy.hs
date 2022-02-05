{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.WQOConstraints.Lazy (
      lazyOC
    , addConstraint
    , intersect
    , isSatisfiable
    , noConstraints
    , union
    , unsatisfiable
    , LazyOC
    ) where

import Text.Printf
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Set as S

import qualified Language.REST.Internal.WQO as WQO
import qualified Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.ADT as ADT

type WQO = WQO.WQO

-- Partially lazy ordering constraints:
-- thunks computation after showing satisfiability

type Thunk a = ADT.ConstraintsADT a

data LazyOC a =
    Unsat
  | Sat (WQO a) (Thunk a)
  deriving (Eq, Ord, Generic, Hashable)

getOrdering :: LazyOC a -> Maybe (WQO a)
getOrdering (Sat wqo _) = Just wqo
getOrdering _           = Nothing

eval :: (Eq a, Ord a, Hashable a) => ADT.ConstraintsADT a -> LazyOC a
eval (ADT.Sat w)   = Sat w ADT.Unsat
eval ADT.Unsat     = Unsat
eval (ADT.Union lhs rhs) =
  case eval t1 of
    Sat w t1' -> Sat w (ADT.union t1' t2)
    Unsat     -> eval t2
  where
    (t1, t2) = (lhs, rhs)
      -- if ADT.minDepth lhs < ADT.minDepth rhs
      -- then (lhs, rhs)
      -- else (rhs, lhs)

eval (ADT.Intersect t1 t2)       =
  case (eval t1, eval t2) of
    (Sat c1 t1', Sat c2 t2') ->
      let
        rest =
          (ADT.intersect (ADT.Sat c1) t2') `ADT.union`
          (ADT.intersect (ADT.Sat c2) t1') `ADT.union`
          (ADT.intersect t1' t2')
      in
        case WQO.merge c1 c2 of
          Just c' -> Sat c' rest
          Nothing -> eval rest
    _ -> Unsat


toADT :: Eq a => LazyOC a -> ADT.ConstraintsADT a
toADT Unsat     = ADT.Unsat
toADT (Sat w r) = ADT.union (ADT.Sat w) r

instance (Show a, Eq a, Ord a, Hashable a) => Show (LazyOC a) where
  show Unsat     = "⊥"
  show (Sat s r) = printf "%s ∨ lazy(%s)" (show s) (show r)

noConstraints :: LazyOC a
noConstraints = Sat (WQO.empty) ADT.Unsat

unsatisfiable :: LazyOC a
unsatisfiable = Unsat

union :: Eq a => LazyOC a -> LazyOC a -> LazyOC a
union Unsat s                 = s
union s Unsat                 = s
union (Sat s _)    _          | s == WQO.empty = noConstraints
union _           (Sat s _)   | s == WQO.empty = noConstraints
union (Sat s1 r1) (Sat s2 r2) = Sat s1 (ADT.union (ADT.Sat s2) (ADT.union r1 r2))

intersect :: (Ord a, Hashable a) => LazyOC a -> LazyOC a -> LazyOC a
intersect t1 t2 = eval $ ADT.intersect (toADT t1) (toADT t2)

isSatisfiable :: LazyOC a -> Bool
isSatisfiable (Sat _ _) = True
isSatisfiable Unsat     = False

singleton :: WQO a -> LazyOC a
singleton c = Sat c ADT.Unsat

relevantConstraints
  :: (Eq a, Ord a, Hashable a) => LazyOC a -> S.Set a -> S.Set a -> LazyOC a
relevantConstraints c _ _ = c

notStrongerThan :: (Monad m, Eq a) => LazyOC a -> LazyOC a -> m Bool
notStrongerThan _     Unsat = return True
notStrongerThan t1    t2    = return $ t1 == t2

addConstraint :: (Ord a, Hashable a) => ADT.WQO a -> LazyOC a -> LazyOC a
addConstraint o c = eval $ ADT.addConstraint o (toADT c)

permits :: (Ord a, Hashable a) => LazyOC a -> WQO.WQO a -> Bool
permits Unsat _            = False
permits (Sat s1 thunk) wqo = s1 `WQO.notStrongerThan` wqo || permits (eval thunk) wqo

lazyOC :: Monad m => OC.WQOConstraints LazyOC m
lazyOC = OC.OC
  addConstraint
  intersect
  (return . isSatisfiable)
  notStrongerThan
  noConstraints
  permits
  relevantConstraints
  union
  unsatisfiable
  undefined
  getOrdering
  id
