{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}

-- | This module contains the implementation of the Recursive Path Quasi-Ordering,
--   defined in section 4.2.1 of the REST paper
module Language.REST.RPO (rpo, rpoGTE, synGTE, rpoSMT, rpoSMTRel) where

import Prelude hiding (EQ, GT)

import Control.Monad.Identity
import Control.Monad.State.Strict
import GHC.Generics
import Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import qualified Data.List as L

import qualified Language.REST.Internal.MultiSet as MS
import           Language.REST.Op
import           Language.REST.Internal.OpOrdering as OpOrdering
import           Language.REST.WQOConstraints as OC
import qualified Language.REST.MetaTerm as MT
import           Language.REST.Types
import qualified Language.REST.RuntimeTerm as RT
import           Language.REST.Internal.MultisetOrder
import           Language.REST.Internal.Util
import           Language.REST.SMT

instance Show RuntimeTerm where
  show (App op trms) =
      if MS.null trms
      then show op
      else show op ++ show trms

instance MT.ToMetaTerm RuntimeTerm where
  toMetaTerm (App op xs) = MT.RWApp op (map MT.toMetaTerm $ MS.toList xs)

type MultiSet = MS.MultiSet

data RuntimeTerm = App Op (MultiSet RuntimeTerm) deriving (Generic, Eq, Hashable, Ord)

rpoTerm :: RT.RuntimeTerm -> RuntimeTerm
rpoTerm (RT.App f xs) = App f $ MS.fromList (map rpoTerm xs)

isSubtermOf :: RuntimeTerm -> RuntimeTerm -> Bool
isSubtermOf t u@(App _ us) = t == u || any (t `isSubtermOf`) (MS.distinctElems us)

type CacheKey oc = (oc Op, Relation, RuntimeTerm, RuntimeTerm)

type Cache oc = M.HashMap (CacheKey oc) (oc Op)

data RPOState oc =
  RPOState
    { rpoCache :: Cache oc
    , rpoDepth :: Int
    }

type RMonad oc = State (RPOState oc)

incDepth :: RMonad oc (oc Op) -> RMonad oc (oc Op)
incDepth action = do
  modify (\st -> st{rpoDepth = rpoDepth st + 1})
  result <- action
  modify (\st -> st{rpoDepth = rpoDepth st - 1})
  return result


cached :: (Eq (oc Op), Hashable (oc Op)) => CacheKey oc -> RMonad oc (oc Op) -> RMonad oc (oc Op)
cached key thunk = do
  cache <- gets rpoCache
  case M.lookup key cache of
    Just result -> return result
    Nothing     -> do
      result <- thunk
      modify (\st -> st{ rpoCache = M.insert key result (rpoCache st)})
      return result

-- | The constraint generator for RPQO. That is, given terms @t@, @u@, @rpo@ generates
--   the constraints on n RPQO ≥ᵣ such that t ≥ᵣ u.
rpo :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RT.RuntimeTerm Identity
rpo = runStateConstraints (cmapConstraints rpoTerm rpo') (RPOState M.empty 0)

rpoMul :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op (MultiSet RuntimeTerm) (RMonad oc)
rpoMul = multisetOrder rpo'

rpo' :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm (RMonad oc)
rpo' impl _      oc _  _ | oc == unsatisfiable impl           = return $ unsatisfiable impl
rpo' OC{unsatisfiable} r oc      t u      | t == u            = return $ if r == GT then unsatisfiable else oc
rpo' OC{unsatisfiable} _ _       t u      | t `isSubtermOf` u = return unsatisfiable
rpo' OC{unsatisfiable} r oc      t u      | u `isSubtermOf` t = return $ if r == EQ then unsatisfiable else oc
rpo' oc r cs (App f ts) (App g us)        | f == g            = rpoMul oc r cs ts us

rpo' oc r cs t@(App f ts) u@(App g us) = incDepth result
  where
    cs'    = noConstraints oc
    result = cached (cs, r, t, u) (intersect oc cs <$> result')
    result' = cached (cs', r, t, u) $
      if r == EQ
      then rpoMul oc r (addConstraint oc (f =. g) cs') ts us
      else
        unionAll oc <$> sequence [
          rpoMul oc GT  (addConstraint oc (f >. g) cs') (MS.singleton t) us
        , rpoMul oc r   (addConstraint oc (f =. g) cs') ts               us
        , rpoMul oc GTE cs'                             ts               (MS.singleton u)
        ]

-- | @rpoGTE impl t u@ generates the constraints a WQO over 'Op' (via @impl@) that ensures
--   that t ≥ᵣ u in the result RPQO ≥ᵣ.
rpoGTE
  :: (?impl::WQOConstraints oc m, Hashable (oc Op), Eq (oc Op), Show (oc Op))
  => RT.RuntimeTerm
  -> RT.RuntimeTerm
  -> oc Op
rpoGTE t u = runIdentity $ rpoGTE' ?impl (noConstraints ?impl) t u

rpoGTE'
  :: (Show (oc Op), Eq (oc Op), Hashable (oc Op))
  => WQOConstraints oc m'
  -> oc Op
  -> RT.RuntimeTerm
  -> RT.RuntimeTerm
  -> Identity (oc Op)
rpoGTE' impl = rpo impl GTE


-- Non symbolic version

synEQ :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
synEQ o l r = synGTE' o l r && synGTE' o r l

-- | Performs the (concrete) RPQO calculation. @synGTE o t u@ returns
--   true iff t ≥ᵣ u using an RPQO with precedence @o@.
synGTE :: OpOrdering -> RT.RuntimeTerm -> RT.RuntimeTerm -> Bool
synGTE o t u = synGTE' o (rpoTerm t) (rpoTerm u)

synGTE' :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
synGTE' ordering t@(App f _ts) (App g us)
  | opGT ordering f g
  = synGTM ordering (MS.singleton t) us
synGTE' ordering (App f ts) (App g us)
  | opEQ ordering f g
  = synGTEM ordering ts us
synGTE' ordering (App _ ts) u = synGTEM ordering ts (MS.singleton u)

rpoT :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
rpoT o t1 t2 = synGTE' o t1 t2 && not (synGTE' o t2 t1)

synGTEM :: OpOrdering -> MultiSet RuntimeTerm -> MultiSet RuntimeTerm -> Bool
synGTEM ordering xs ys = case removeEqBy (synEQ ordering) (MS.toList xs) (MS.toList ys) of
  (xs', ys') -> all (\y -> any (\x -> rpoT ordering x y) xs') ys'

synGTM :: OpOrdering -> MultiSet RuntimeTerm -> MultiSet RuntimeTerm -> Bool
synGTM ordering xs ys = case removeEqBy (synEQ ordering) (MS.toList xs) (MS.toList ys) of
  ([] , [] ) -> False
  (xs', ys') -> all (\y -> any (\x -> rpoT ordering x y) xs') ys'

-- | Direct SMT translation of the RPO constraint generator, without going
--   through 'WQOConstraints'. @rpoSMTRel r t u@ is the SMT constraint on the
--   weights of function symbols such that @t@ is related to @u@ by @r@ in the
--   recursive path ordering.
rpoSMTRel :: Relation -> RT.RuntimeTerm -> RT.RuntimeTerm -> SMTExpr Bool
rpoSMTRel r t u = rpoSMT' r smtTrue (rpoTerm t) (rpoTerm u)

-- | The GTE instance of 'rpoSMTRel'.
rpoSMT :: RT.RuntimeTerm -> RT.RuntimeTerm -> SMTExpr Bool
rpoSMT = rpoSMTRel GTE

rpoSMT' :: Relation -> SMTExpr Bool -> RuntimeTerm -> RuntimeTerm -> SMTExpr Bool
rpoSMT' r e t u
  | t == u                              = if r == GT then smtFalse else e
  | t `isSubtermOf` u                   = smtFalse
  | u `isSubtermOf` t                   = if r == EQ then smtFalse else e
  | App f ts <- t, App g us <- u, f == g = rpoMulSMT r e ts us
  | App f ts <- t, App g us <- u         =
      let eqPrec = smtEqual [toSMT f, toSMT g]
          gtPrec = Greater (toSMT f) (toSMT g)
          body
            | r == EQ   = rpoMulSMT r eqPrec ts us
            | otherwise = smtOr
                             [ rpoMulSMT GT gtPrec (MS.singleton t) us
                             , rpoMulSMT r  eqPrec ts us
                             , rpoMulSMT GTE smtTrue ts (MS.singleton u)
                             ]
      in smtAnd [e, body]

rpoMulSMT :: Relation -> SMTExpr Bool -> MultiSet RuntimeTerm -> MultiSet RuntimeTerm -> SMTExpr Bool
rpoMulSMT r e ts0 us0 = uncurry go (removeEQs ts0 us0)
  where
    go ts us
      | MS.null ts && MS.null us       = if r == GT then smtFalse else e
      | not (MS.null ts) && MS.null us = if r == EQ then smtFalse else e
      | MS.null ts && not (MS.null us) = smtFalse
      | otherwise = smtOr [ posConstraints pos1 | pos1 <- HS.toList (possibilities r (MS.toList ts) (MS.toList us)) ]

    posConstraints pos1 = L.foldl' apply e (HS.toList pos1)
      where
        apply acc (ReplaceOne t u) = rpoSMT' EQ acc t u
        apply acc (Replace t ts')  =
          if HS.null ts'
            then acc
            else smtAnd $ acc : map (rpoSMT' GT smtTrue t) (HS.toList ts')
