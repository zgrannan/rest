

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}




module Language.REST.LPO (lpo, lpoStrict, lpoSMT, lpoStrictSMT, lpoSMTRel, lpoStrictSMTRel) where

import Prelude hiding (EQ, GT, lex)

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Hashable
import qualified Data.HashMap.Strict as M

import           Language.REST.Op
import           Language.REST.Internal.OpOrdering as OpOrdering
import           Language.REST.WQOConstraints as OC
import           Language.REST.Types
import           Language.REST.RuntimeTerm
import           Language.REST.SMT

lex
  :: (Eq a, Ord b, Hashable b)
  => WQOConstraints impl m
  -> Bool
  -> impl b
  -> (WQOConstraints impl m -> Relation -> impl b -> a -> a -> impl b)
  -> [a]
  -> [a]
  -> impl b
lex oc strict cs f (t:ts) (u:us) | t == u = lex oc strict cs f ts us
lex oc strict cs f (t:ts) (u:us) = union oc case1 case2
  where
    -- t > u
    case1 = f oc GT cs t u
    -- t = u
    case2 =
      let
        cs' = f oc EQ cs t u
      in
        lex oc strict cs' f ts us
lex oc _      _  _ []    (_:_) = unsatisfiable oc
lex _  _      cs _ (_:_) []    = cs
lex oc strict cs _ []    []    = if strict then unsatisfiable oc else cs

lpo' :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) =>
  Bool -> WQOConstraints oc m -> Relation -> oc Op -> RuntimeTerm -> RuntimeTerm -> oc Op
-- lpo' False oc EQ cs t u = intersect oc (lpo' False oc GTE cs t u) (lpo' False oc GTE cs u t)
lpo' False oc EQ _cs (App _f ts) (App _g us) | length ts /= length us = unsatisfiable oc
lpo' False oc EQ cs (App f ts) (App g us) =
  let
    cs'  = intersect oc cs (singleton oc $ f =. g)
    subs = zipWith (lpo' False oc EQ cs') ts us
  in
    intersectAll oc (cs' : subs)

lpo' True  oc EQ cs t u = if t == u then cs else unsatisfiable oc
lpo' strict oc r cs t@(App f ts) u@(App g us) = result
  where
    result  = intersect oc cs result'
    result' = unionAll oc [case1, case2, case3]

    -- tᵢ ≥ u for some i
    case1 = unionAll oc (map go ts) where
      go ti = lpo' strict oc GTE cs ti u

    -- f > g ∧ t > uⱼ for all j
    case2 =
      if f == g
      then unsatisfiable oc
      else intersect oc tDominatesUs (singleton oc $ f >. g)

    -- f = g ∧ t > uⱼ for all j ts >lex us
    case3 =
      if strict && f /= g
      then unsatisfiable oc
      else intersectAll oc ([tDominatesUs, lex oc (r == GT) cs (lpo' strict) ts us] ++ symEQ) where
        symEQ = [singleton oc (f =. g) | f /= g]


    tDominatesUs = intersectAll oc (map go us) where
      go = lpo' strict oc GT cs t


-- | Constraint generator for a quasi-order extension to the Lexicographic path ordering
lpo :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm Identity
lpo oc r cs t u = return $ lpo' False oc r cs t u

-- | Constraint generator for a strict version of the quasi-order extension to
--   the Lexicographic path ordering.
lpoStrict :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm Identity
lpoStrict oc r cs t u = return $ lpo' True oc r cs t u

-- | Direct SMT translation of the LPO constraint generator, without going
--   through 'WQOConstraints'. The returned bindings are the memoized cores
--   that the caller should intern.
type LPOKey = (Bool, Relation, SMTExpr Bool, RuntimeTerm, RuntimeTerm)

data LPMCache = LPMCache
  { lpMemo  :: M.HashMap LPOKey (SMTExpr Bool)
  , lpBinds :: [SMTExpr Bool]
  }

emptyLPMCache :: LPMCache
emptyLPMCache = LPMCache M.empty []

memoLPO :: LPOKey -> State LPMCache (SMTExpr Bool) -> State LPMCache (SMTExpr Bool)
memoLPO key thunk = do
  st <- get
  case M.lookup key (lpMemo st) of
    Just v  -> pure v
    Nothing -> do
      v <- thunk
      modify (\s -> s { lpMemo = M.insert key v (lpMemo s), lpBinds = lpBinds s ++ [v] })
      pure v

lpoSMT' :: Bool -> Relation -> SMTExpr Bool -> RuntimeTerm -> RuntimeTerm -> State LPMCache (SMTExpr Bool)
lpoSMT' strict r e t@(App f ts) u@(App g us) =
  memoLPO (strict, r, e, t, u) $
    if | not strict && r == EQ && length ts /= length us -> pure smtFalse
       | not strict && r == EQ -> do
           let cs' = smtAnd [e, eqPrec f g]
           subs <- mapM (\(a, b) -> lpoSMT' strict EQ cs' a b) (zip ts us)
           pure (smtAnd (cs' : subs))
       | strict && r == EQ -> pure (if t == u then e else smtFalse)
       | otherwise -> do
           c1 <- case1
           c2 <- case2
           c3 <- case3
           pure (smtAnd [e, smtOr [c1, c2, c3]])
  where
    case1 = smtOr <$> mapM (\ti -> lpoSMT' strict GTE e ti u) ts
    case2 = if f == g then pure smtFalse else do
      td <- tDominatesUs
      pure (smtAnd [td, gtPrec f g])
    case3 = if strict && f /= g then pure smtFalse else do
      td <- tDominatesUs
      lx <- lexSMT (r == GT) e ts us
      pure (smtAnd ([td, lx] ++ [eqPrec f g | f /= g]))

    tDominatesUs = smtAnd <$> mapM (\uj -> lpoSMT' strict GT e t uj) us

    eqPrec :: Op -> Op -> SMTExpr Bool
    eqPrec a b = smtEqual [toSMT a, toSMT b]

    gtPrec :: Op -> Op -> SMTExpr Bool
    gtPrec a b = Greater (toSMT a) (toSMT b)

    lexSMT :: Bool -> SMTExpr Bool -> [RuntimeTerm] -> [RuntimeTerm] -> State LPMCache (SMTExpr Bool)
    lexSMT isGT acc [] (_:_) = pure smtFalse
    lexSMT _   acc (_:_) [] = pure acc
    lexSMT isGT acc []    [] = pure (if isGT then smtFalse else acc)
    lexSMT isGT acc (x:xs) (y:ys)
      | x == y = lexSMT isGT acc xs ys
      | otherwise = do
          a    <- lpoSMT' strict GT acc x y
          acc' <- lpoSMT' strict EQ acc x y
          b    <- lexSMT isGT acc' xs ys
          pure (smtOr [a, b])

-- | The GTE instance of 'lpoSMT'' for the non-strict ordering.
lpoSMTRel :: Relation -> RuntimeTerm -> RuntimeTerm -> SMTResult
lpoSMTRel r t u =
  let (e, st) = runState (lpoSMT' False r smtTrue t u) emptyLPMCache
  in SMTResult e (lpBinds st)

lpoSMT :: RuntimeTerm -> RuntimeTerm -> SMTResult
lpoSMT = lpoSMTRel GTE

-- | The GTE instance of 'lpoSMT'' for the strict ordering.
lpoStrictSMTRel :: Relation -> RuntimeTerm -> RuntimeTerm -> SMTResult
lpoStrictSMTRel r t u =
  let (e, st) = runState (lpoSMT' True r smtTrue t u) emptyLPMCache
  in SMTResult e (lpBinds st)

lpoStrictSMT :: RuntimeTerm -> RuntimeTerm -> SMTResult
lpoStrictSMT = lpoStrictSMTRel GTE
