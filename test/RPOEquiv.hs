{-# LANGUAGE OverloadedStrings #-}

-- | Checks that the direct SMT translation of RPO ('rpoSMTRel') is logically
--   equivalent to the SMT translation obtained by running the polymorphic
--   'rpo' generator with the 'SMTConstraint' implementation.
module RPOEquiv where

import Prelude hiding (GT, EQ)

import Control.Monad.Identity (runIdentity)

import Language.REST.SMT
import Language.REST.Types (Relation(..))
import Language.REST.RuntimeTerm
import Language.REST.RPO (rpoSMTRel, rpo)
import Language.REST.WQOConstraints as OC
import Language.REST.WQOConstraints.SMT (smtOC)
import Nat () -- IsString RuntimeTerm

tests :: SolverHandle -> [(String, IO Bool)]
tests z3 =
  [ (label, equiv z3 (srExpr (rpoSMTRel r t u)) (ref z3 r t u))
  | (label, r, t, u) <- cases
  ]
  where
    cases :: [(String, Relation, RuntimeTerm, RuntimeTerm)]
    cases =
      [ ("eq-gte"          , GTE, "f(a)"          , "f(a)")
      , ("subterm-l"       , GTE, "f(a)"          , "a")
      , ("subterm-r"       , GTE, "a"             , "f(a)")
      , ("head-same"       , GTE, "f(a, b)"       , "f(b, a)")
      , ("head-diff"       , GTE, "f(a)"          , "g(a)")
      , ("nested"          , GTE, "f(g(a), b)"    , "f(h(a), c)")
      , ("deep"            , GTE, "f(g(h(a)))"    , "g(a)")
      , ("two-args"        , GTE, "f(a, g(b))"    , "f(g(c), d)")
      , ("empty-left"      , GTE, "f"             , "f(a)")
      , ("empty-right"     , GTE, "f(a)"          , "f")
      , ("empty-both"      , GTE, "f"             , "f")
      , ("gt-equal"        , GT , "f(a)"          , "f(a)")
      , ("eq-equal"        , EQ , "f(a)"          , "f(a)")
      , ("gt-subterm-r"    , GT , "a"             , "f(a)")
      ]

-- | Reference: the polymorphic RPO generator instantiated with 'SMTConstraint'.
ref :: SolverHandle -> Relation -> RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
ref z3 r t u = toSMT (runIdentity (rpo (smtOC z3) r (OC.noConstraints (smtOC z3)) t u))

notE :: SMTExpr Bool -> SMTExpr Bool
notE p = smtImplies p smtFalse

iff :: SMTExpr Bool -> SMTExpr Bool -> SMTExpr Bool
iff a b = smtAnd [smtImplies a b, smtImplies b a]

equiv :: SolverHandle -> SMTExpr Bool -> SMTExpr Bool -> IO Bool
equiv z3 e1 e2 = not <$> checkSat' z3 (notE (iff e1 e2))
