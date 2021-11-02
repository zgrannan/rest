{-# LANGUAGE FlexibleContexts #-}

module Language.REST.KBO (kbo, kboGTE) where

import Control.Monad.Identity

import qualified Data.List as L

import           Language.REST.AbstractOC
import qualified Language.REST.MultiSet as MS
import           Language.REST.Op
import           Language.REST.Types hiding (GTE)
import           Language.REST.RuntimeTerm as RT
import           Language.REST.SMT
import           Language.REST.Util

type MultiSet = MS.MultiSet

termOps :: RuntimeTerm -> [Op]
termOps (App f xs) = f:(concatMap termOps xs)

kboGTE :: RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
kboGTE t u = allGT0 `smtAnd` (size tOps `smtGTE` size uOps)
  where
    (tOps, uOps) = removeEqBy (==) (termOps t) (termOps u)
    uniqOps      = L.nub (tOps ++ uOps)
    allGT0       = And (map gt0 uniqOps)
    gt0 op       = toSMT op `Greater` (Const 0)
    size ops     = smtAdd (map toSMT ops)


kbo :: SolverHandle -> AbstractOC (SMTExpr Bool) RuntimeTerm IO
kbo solver = AbstractOC
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
