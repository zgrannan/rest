{-# LANGUAGE FlexibleContexts #-}

module Language.REST.KBO (kbo, kboGTE) where

import           Language.REST.OCAlgebra
import           Language.REST.Op
import           Language.REST.RuntimeTerm as RT
import           Language.REST.SMT
import           Language.REST.Internal.Util

import qualified Data.Map as M

type MultiSet = MS.MultiSet

termOps :: RuntimeTerm -> [Op]
termOps (App f xs) = f:(concatMap termOps xs)

arityConstraints :: RuntimeTerm -> SMTExpr Bool
arityConstraints t = toExpr $ go M.empty t where
  go :: M.Map Op Int -> RuntimeTerm -> M.Map Op Int
  go m (App f [])  = M.insert f 1 m
  go m (App f [t]) = go (M.insert f 1 m) t
  go m (App f ts)  = foldl go (M.insert f 0 m) ts

  toExpr m = And $ map toConstraint (M.toList m)
  toConstraint (sym, n) = toSMT sym `smtGTE` (Const n)


kboGTE :: RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
kboGTE t u = arityConstraints t `smtAnd` arityConstraints u `smtAnd` (size tOps `smtGTE` size uOps)
  where
    (tOps, uOps) = removeEqBy (==) (termOps t) (termOps u)
    size ops     = smtAdd (map toSMT ops)


kbo :: SolverHandle -> OCAlgebra (SMTExpr Bool) RuntimeTerm IO
kbo solver = OCAlgebra
  {  isSat           = checkSat' solver
  ,  refine          = refine
  ,  top             = smtTrue
  ,  union           = union
  ,  notStrongerThan = notStrongerThan
  }
  where
    union  e1 e2          = Or [e1, e2]
    refine e t u          = e `smtAnd` kboGTE t u
    notStrongerThan e1 e2 = checkSat' solver (Implies e2 e1)
