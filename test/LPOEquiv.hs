{-# LANGUAGE OverloadedStrings #-}

-- | Checks that the direct SMT translations of LPO ('lpoSMTRel' and
--   'lpoStrictSMTRel') are logically equivalent to the SMT translations obtained
--   by running the polymorphic 'lpo'/'lpoStrict' generators with 'SMTConstraint'.
module LPOEquiv where

import Prelude hiding (GT, EQ)

import Control.Monad.Identity (runIdentity)

import Language.REST.SMT
import Language.REST.Types (Relation(..))
import Language.REST.RuntimeTerm
import Language.REST.LPO (lpoSMTRel, lpoStrictSMTRel, lpo, lpoStrict)
import Language.REST.WQOConstraints as OC
import Language.REST.WQOConstraints.SMT (smtOC)
import Nat () -- IsString RuntimeTerm

tests :: SolverHandle -> [(String, IO Bool)]
tests z3 =
  [ (label, equiv z3 (step strict r t u) (ref z3 strict r t u))
  | (label, strict, r, t, u) <- cases
  ]
  where
    step :: Bool -> Relation -> RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
    step True  = lpoStrictSMTRel
    step False = lpoSMTRel

    cases :: [(String, Bool, Relation, RuntimeTerm, RuntimeTerm)]
    cases =
      [ ("gte-same"        , False, GTE, "f(a)"      , "f(a)")
      , ("gte-lex"         , False, GTE, "f(a, b)"   , "f(b, a)")
      , ("lex-skip-apply"  , False, GTE, "f(a, b)"   , "f(a, c)")
      , ("gte-diff-head"   , False, GTE, "f(a)"      , "g(a)")
      , ("eq-diff-arity"   , False, EQ , "f(a)"      , "g(a, b)")
      , ("eq-same-arity"   , False, EQ , "f(a)"      , "g(b)")
      , ("gt-same"         , False, GT , "f(a)"      , "f(a)")
      , ("strict-gte"      , True , GTE, "f(a, b)"   , "f(b, a)")
      , ("strict-eq-same"  , True , EQ , "f(a)"      , "f(a)")
      , ("strict-eq-diff"  , True , EQ , "f(a)"      , "g(a)")
      , ("strict-gt"       , True , GT , "f(a, b)"   , "f(b, a)")
      , ("strict-lex-skip"  , True , GTE, "f(a, b)"   , "f(a, c)")
      , ("empty-left"      , False, GTE, "f"         , "f(a)")
      , ("empty-right"     , False, GTE, "f(a)"      , "f")
      , ("empty-both"      , False, GTE, "f"         , "f")
      ]

-- | Reference: the polymorphic LPO generator instantiated with 'SMTConstraint'.
ref :: SolverHandle -> Bool -> Relation -> RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
ref z3 strict r t u =
  toSMT (runIdentity (gen strict (smtOC z3) r (OC.noConstraints (smtOC z3)) t u))
  where
    gen True  = lpoStrict
    gen False = lpo

notE :: SMTExpr Bool -> SMTExpr Bool
notE p = smtImplies p smtFalse

iff :: SMTExpr Bool -> SMTExpr Bool -> SMTExpr Bool
iff a b = smtAnd [smtImplies a b, smtImplies b a]

equiv :: SolverHandle -> SMTExpr Bool -> SMTExpr Bool -> IO Bool
equiv z3 e1 e2 = not <$> checkSat' z3 (notE (iff e1 e2))
