{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.REST.MultisetOrder (multisetOrder, possibilities) where

import GHC.Generics
import qualified Data.List as L
import Debug.Trace (trace)
import Prelude hiding (EQ, GT)
import Data.Hashable
import qualified Data.HashSet as S
import Text.Printf

import qualified Language.REST.MultiSet as M
import Language.REST.OrderingConstraints as OC
import Language.REST.Types

type MultiSet = M.MultiSet

trace' :: String -> a -> a
-- trace' = trace
trace' _ x = x

removeEQs :: (Eq x, Ord x, Hashable x) => MultiSet x -> MultiSet x -> (MultiSet x, MultiSet x)
removeEQs ts0 us0 = go (M.toList ts0) M.empty us0 where
  go []       ts us                   = (ts, us)
  go (x : xs) ts us | x `M.member` us = go xs ts (M.delete x us)
  go (x : xs) ts us | otherwise       = go xs (M.insert x ts) us

data Replace a =
    ReplaceOne a a
  | Replace a (S.HashSet a)
  deriving (Eq, Hashable, Generic, Show)

powerset []      = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

possibilities :: (Hashable a, Eq a) => Relation -> [a] -> [a] -> S.HashSet (S.HashSet (Replace a))
possibilities r []     []    = if r == GT then S.empty else S.singleton (S.empty)
possibilities r xs     []    = if r == EQ then S.empty else S.singleton (S.fromList $ map (flip Replace S.empty)  xs)
possibilities _ []     (_:_) = S.empty
possibilities r (x:xs) ys    = if r == EQ then eqs else S.union eqs doms where
  eqs = S.unions $ map go ys where
    go y = S.map (S.insert (ReplaceOne x y)) (possibilities r xs (L.delete y ys))
  doms = S.unions $ map go (powerset $ L.nub ys) where
    go ys' = S.map
      (S.insert (Replace x (S.fromList ys')))
      (possibilities GTE xs (filter (not . flip elem ys') ys))


multisetOrder :: forall oc base lifted m . (Ord lifted, Ord base, Show base, Eq base, Hashable base, Hashable lifted, Eq lifted, Show (oc base), Eq (oc base),  Monad m) =>
     ConstraintGen oc base lifted m
  -> ConstraintGen oc base (MultiSet lifted) m
multisetOrder _          impl _ oc _   _   | oc == unsatisfiable impl = return $ unsatisfiable impl
multisetOrder underlying impl r oc ts0 us0 = (uncurry go) (removeEQs ts0 us0) where
  go :: MultiSet lifted -> MultiSet lifted -> m (oc base)
  go ts us | M.null ts && M.null us             = return $ if r == GT then unsatisfiable impl else oc
  go ts us | not (M.null ts) && M.null us       = return $ if r == EQ then unsatisfiable impl else oc
  go ts us | M.null ts       && not (M.null us) = return $ unsatisfiable impl
  go ts us = result
    where

      pos = possibilities r (M.toList ts) (M.toList us)

      result =
        trace' ("There are " ++ (show $ S.size pos) ++ " possibilities") $
        unionAll impl <$> mapM posConstraints (S.toList pos)

      posConstraints pos1 = L.foldl' apply (return oc) (S.toList pos1) where
        apply moc (ReplaceOne t u) = do
          oc' <- moc
          underlying impl EQ oc' t u
        apply moc (Replace t ts') = do
          oc' <- moc
          if S.null ts'
            then return oc'
            else intersectAll impl <$> (mapM (underlying impl GT oc' t) (S.toList ts'))
