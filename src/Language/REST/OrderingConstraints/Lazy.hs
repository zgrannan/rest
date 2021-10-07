{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.REST.OrderingConstraints.Lazy (
      lazyOC
    , addConstraint
    , intersect
    , isSatisfiable
    , noConstraints
    , union
    , unsatisfiable
    , LazyOC
    ) where

import Debug.Trace
import Text.Printf
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S

import qualified Language.REST.WQO as WQO
import qualified Language.REST.OrderingConstraints as OC
import qualified Language.REST.OrderingConstraints.ADT as ADT

type WQO = WQO.WQO

-- Partially lazy ordering constraints:
-- thunks computation after showing satisfiability

type Thunk a = ADT.ConstraintsADT a

data LazyOC a =
    Unsat
  | Sat (WQO a) (Thunk a)
  deriving (Eq, Ord, Generic, Hashable)

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


toADT Unsat     = ADT.Unsat
toADT (Sat w r) = ADT.union (ADT.Sat w) r

instance (Show a, Eq a, Ord a, Hashable a) => Show (LazyOC a) where
  show Unsat     = "⊥"
  show (Sat s r) = printf "%s ∨ lazy(%s)" (show s) (show r)

noConstraints = Sat (WQO.empty) ADT.Unsat
unsatisfiable = Unsat

union Unsat s                 = s
union s Unsat                 = s
union (Sat s _)    _          | s == WQO.empty = noConstraints
union _           (Sat s _)   | s == WQO.empty = noConstraints
union (Sat s1 r1) (Sat s2 r2) = Sat s1 (ADT.union (ADT.Sat s2) (ADT.union r1 r2))

intersect t1 t2 = eval $ ADT.intersect (toADT t1) (toADT t2)

isSatisfiable (Sat _ _) = True
isSatisfiable Unsat     = False

singleton c = Sat c ADT.Unsat

relevantConstraints c _ _ = c

notStrongerThan _     Unsat = return True
notStrongerThan t1    t2    = return $ t1 == t2

addConstraint o c = eval $ ADT.addConstraint o (toADT c)

permits Unsat _            = False
permits (Sat s1 thunk) wqo = s1 `WQO.notStrongerThan` wqo || permits (eval thunk) wqo

lazyOC :: Monad m => OC.OrderingConstraints LazyOC m
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
