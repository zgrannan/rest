{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

#define OPTIMIZE_WQO

module Language.REST.OrderingConstraints.ADT where

import GHC.Generics (Generic)

import Debug.Trace
import Data.Hashable
import Control.Monad.State.Lazy
import qualified Data.Set as S
import qualified Data.Maybe as Mb
import qualified Data.Map.Strict as M
import qualified Language.REST.WQO as WQO
import qualified Language.REST.OrderingConstraints as OC
import Language.REST.SMT
import Language.REST.Op
import System.IO.Unsafe
import Text.Printf

type WQO = WQO.WQO

data ConstraintsADT a =
    Sat (WQO a)
  | Unsat
  | Union (ConstraintsADT a) (ConstraintsADT a)
  | Intersect (ConstraintsADT a) (ConstraintsADT a)
  deriving (Eq, Ord, Generic, Hashable)

instance {-# OVERLAPPING #-} (ToSMTVar a Int) => ToSMT (ConstraintsADT a) Bool where
  toSMT (Sat w)           = toSMT w
  toSMT Unsat             = smtFalse
  toSMT (Union w1 w2)     = Or  [toSMT w1, toSMT w2]
  toSMT (Intersect w1 w2) = And [toSMT w1, toSMT w2]

{-# SPECIALIZE cost :: ConstraintsADT Op -> Int #-}
cost :: (Ord a, Eq a, Hashable a) => ConstraintsADT a -> Int
cost (Union lhs rhs)     = min (cost lhs) (cost rhs)
cost (Intersect lhs rhs) = cost lhs + cost rhs
cost (Sat wqo)           = S.size $ WQO.elems wqo
cost Unsat               = 100

minDepth :: ConstraintsADT a -> Int
minDepth (Union lhs rhs)     = 1 + min (minDepth lhs) (minDepth rhs)
minDepth (Intersect lhs rhs) = 1 + min (minDepth lhs) (minDepth rhs)
minDepth _                   = 1

maxDepth :: ConstraintsADT a -> Int
maxDepth (Union lhs rhs)     = 1 + max (maxDepth lhs) (maxDepth rhs)
maxDepth (Intersect lhs rhs) = 1 + max (maxDepth lhs) (maxDepth rhs)
maxDepth _                   = 1

intersect :: (Eq a, Ord a, Hashable a) => ConstraintsADT a -> ConstraintsADT a -> ConstraintsADT a

#ifdef OPTIMIZE_WQO
-- Optimization
intersect (Sat t) (Sat u) =
  case WQO.merge t u of
    Just t' -> Sat t'
    Nothing -> Unsat
#endif

intersect (Sat w) v            | w == WQO.empty = v
intersect v            (Sat w) | w == WQO.empty = v
intersect _ Unsat     = Unsat
intersect Unsat _     = Unsat
intersect t1 t2 | t1 == t2 = t1
intersect t1 (Union t2 t3) | t1 == t2 || t1 == t3 = t1
#ifdef OPTIMIZE_WQO
intersect (Sat w1) (Intersect (Sat w2) t2) =
  case WQO.merge w1 w2 of
    Just w' -> intersect (Sat w') t2
    Nothing -> Unsat
intersect (Sat w1) (Intersect t2 (Sat w2)) =
  case WQO.merge w1 w2 of
    Just w' -> intersect (Sat w') t2
    Nothing -> Unsat
intersect (Intersect t1 (Sat w1)) (Sat w2) =
  case WQO.merge w1 w2 of
    Just w' -> intersect t1 (Sat w')
    Nothing -> Unsat
intersect (Intersect (Sat w1) t1) (Sat w2) =
  case WQO.merge w1 w2 of
    Just w' -> intersect t1 (Sat w')
    Nothing -> Unsat
#endif
intersect t1 t2            = Intersect t1 t2

union (Sat w) _            | w == WQO.empty = Sat w
union _            (Sat w) | w == WQO.empty = Sat w
union (Intersect a b)  c | a == c || b == c = c
union a (Intersect b c)  | a == b || a == c = a
union a (Union b c)      | a == b           = union a c
union Unsat s     = s
union s Unsat     = s
union c1 c2 | c1 == c2 = c1
union c1 c2            = Union c1 c2

addConstraint o c = intersect (Sat o) c

relevantConstraints c _ _ = c

notStrongerThan t1 t2 | t1 == t2            = smtTrue
notStrongerThan t1 _  | t1 == noConstraints = smtTrue
notStrongerThan t1 t2 | otherwise           = Implies (toSMT t2) (toSMT t1)

noConstraints = Sat (WQO.empty)
unsatisfiable = Unsat

trace' = trace

{-# SPECIALIZE getConstraints :: ConstraintsADT Op -> [WQO Op] #-}
getConstraints :: forall a. (Show a, Ord a, Hashable a) => ConstraintsADT a -> [WQO a]
getConstraints adt = -- trace' ("Get constraints, size : " ++ (show $ dnfSize adt)) $
  evalState (getConstraints' adt) (GCState M.empty M.empty)

data GCState a = GCState {
    cs :: M.Map (ConstraintsADT a) (GCResult a)
  , ms :: M.Map (WQO a, WQO a) (Maybe (WQO a))
}

type GCResult a = [WQO a]

type GCMonad a = State (GCState a) (GCResult a)

cached :: (Ord a) => ConstraintsADT a -> GCMonad a -> GCMonad a
cached key thunk = do
  cache <- gets cs
  case M.lookup key cache of
    Just result -> trace' ("ADT Cache hit") $ return result
    Nothing     -> trace' ("ADT Cache miss") $ do
      result <- trace' "Do thunk" thunk
      trace' "Done" $ modify (\st -> st{cs = M.insert key result (cs st)})
      return result
 where
   trace' _  x = x
   -- trace' = trace

cached' :: (Hashable a, Show a, Ord a) => (WQO a, WQO a) -> Maybe (WQO a) -> State (GCState a) (Maybe (WQO a))
cached' (lhs, rhs) thunk = do
  cache <- gets ms
  case M.lookup (lhs, rhs) cache of
    Just result -> trace' ("WQO Cache hit") $ return result
    Nothing     -> trace' ("WQO Cache miss" ++ show (lhs, rhs)) $ do
      trace' "Done" $ modify (\st -> st{ms = M.insert (rhs, lhs) thunk $ M.insert (lhs, rhs) thunk (ms st)})
      return thunk
 where
   trace' _  x = x
   -- trace' = trace

getConstraints' :: forall a. (Show a, Ord a, Hashable a) => ConstraintsADT a -> State (GCState a) [WQO a]
getConstraints' (Sat w)         = return [w]
getConstraints' Unsat           = return []
getConstraints' c@(Union lhs rhs) =
  cached c $ do
    c1' <- cached c1 $ getConstraints' c1
    c2' <- cached c2 $ getConstraints' c2
    return $ c1' ++ c2'
  where
      (c1, c2) =
        if cost lhs < cost rhs
        then (lhs, rhs)
        else (rhs, lhs)
getConstraints' c@(Intersect lhs rhs) = cached c $ do
  c1' <- cached c1 $ getConstraints' c1
  if null c1'
    then return []
    else (cached c2 $ getConstraints' c2) >>= go c1'
  where
      go :: [WQO a] -> [WQO a] -> State (GCState a) [WQO a]
      go c1' c2' = flatten <$>
        (sequence $ do
          wqo1 <- c1'
          wqo2 <- c2'
          return (cached' (wqo1, wqo2) $ WQO.merge wqo1 wqo2))
      flatten = concatMap Mb.maybeToList
      (c1, c2) =
        if cost lhs > cost rhs
        then (lhs, rhs)
        else (rhs, lhs)

dnfSize :: ConstraintsADT a -> Int
dnfSize (Sat w)       = 1
dnfSize Unsat         = 0
dnfSize (Union w1 w2) = dnfSize w1 + dnfSize w2
dnfSize (Intersect w1 w2) = dnfSize w1 * dnfSize w2

-- toDNF (Union lhs rhs) = S.union (toDNF lhs) (toDNF rhs)
-- toDNF (Intersect lhs rhs) =
--   let
--     ldnf = toDNF lhs
--     rdnf = toDNF rhs
--   in
--     S.unions

simplify adt = undefined
-- simplify adt = case getConstraints adt of
--   []     -> Unsat
--   (x:xs) -> foldl go (Sat x) xs
--   where
--     go a x = Union (Sat x) a

permits adt wqo = any (`WQO.notStrongerThan` wqo) (getConstraints adt)

isSatisfiable :: (ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => ConstraintsADT a -> SMTExpr Bool
isSatisfiable s = toSMT s
  -- trace (show (minDepth s) ++ " " ++ show (maxDepth s)) $ not $ null $ getConstraints s

instance (Eq a, Hashable a,  Show a) => Show (ConstraintsADT a) where
  -- show s = go 0 s where
  --   go n (Sat w)         = indent n $ show w
  --   go n Unsat           = indent n $ "⊥"
  --   go n (Union w t )    = indent n $ printf "∪\n%s\n%s" (go (n+1) w) (go (n+1) t)
  --   go n (Intersect w t) = indent n $ printf "∩\n%s\n%s" (go (n+1) w) (go (n+1) t)

  --   indent 0 s = s
  --   indent n s = take (n - 1) (repeat '|') ++ '+':s

  show (Sat w)         = show w
  show Unsat           = "⊥"
  show (Union w t )    = printf "(%s ∨\n %s)" (show w) (show t)
  show (Intersect w t) = printf "(%s ∧ %s)" (show w) (show t)

adtOC z3 = OC.liftC (checkSat' z3) adtOC'

adtOC' = OC.OC
  addConstraint
  intersect
  isSatisfiable
  notStrongerThan
  noConstraints
  permits
  relevantConstraints
  union
  unsatisfiable
  undefined
  undefined
  simplify
