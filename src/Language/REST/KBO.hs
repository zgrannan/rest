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

type MultiSet = MS.MultiSet

ops :: RuntimeTerm -> [Op]
ops (App f xs) = f:(concatMap ops xs)

termSize :: RuntimeTerm -> SMTExpr Int
termSize t = Add (map toSMT (ops t))

kboGTE :: RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
kboGTE t u = allGT0 `smtAnd` (termSize t `GTE` termSize u)
  where
    uniqOps = L.nub (ops t ++ ops u)
    allGT0  = And (map gt0 uniqOps)
    gt0 op  = toSMT op `Greater` (Const 0)


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
