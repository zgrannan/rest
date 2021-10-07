{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.REST.ConcreteOC where

import qualified Language.REST.AbstractOC as AOC
import qualified Language.REST.WQO as WQO
import           Language.REST.RuntimeTerm
import           Language.REST.RPO
import           Language.REST.OpOrdering
import           Language.REST.MetaTerm

import Data.List as L
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Set as S

data ConcreteOC = ConcreteOC [RuntimeTerm] (Maybe OpOrdering)
  deriving (Eq, Ord, Generic, Hashable)

instance Show ConcreteOC where
  show (ConcreteOC _ (Just oo)) = show oo
  show _                        = "impossible"

isSat (ConcreteOC _ (Just _)) = True
isSat _                       = False

getOrdering ts =
  let
    ops       = S.unions $ map termOps ts
    orderings = S.toList $ WQO.orderings ops
  in
    L.find (`orients` ts) orderings


orients :: OpOrdering -> [RuntimeTerm] -> Bool
orients ordering terms =
  let
    pairs = zip terms (tail terms)
  in
    all (uncurry $ synGTE ordering) pairs

concreteOC :: Monad m => AOC.AbstractOC ConcreteOC RuntimeTerm m
concreteOC = AOC.AbstractOC (return . isSat) refine (ConcreteOC [] (Just (WQO.empty))) union notStrongerThan
  where
    union t1 _ = t1
    notStrongerThan _ _ = return False
    refine :: ConcreteOC -> RuntimeTerm -> RuntimeTerm -> ConcreteOC
    refine (ConcreteOC ts (Just o)) _ u =
      let
        ts' = ts ++ [u]
      in
        ConcreteOC ts' $
          if o `orients` ts'
          then Just o
          else getOrdering ts'
    refine (ConcreteOC ts Nothing) _ u = ConcreteOC (ts ++ [u]) Nothing
