{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Language.REST.Core where

import Prelude hiding (GT, EQ)

import           Debug.Trace                    ( trace )
import           Data.Functor.Identity
import qualified Data.List                     as L
import qualified Data.HashSet                      as S

import           Language.REST.AbstractOC
import           Language.REST.Op
import           Language.REST.OrderingConstraints
import           Language.REST.RPO
import           Language.REST.OpOrdering as OO
import           Language.REST.Types
import qualified Language.REST.MetaTerm as MT
import           Language.REST.WQO
import           Language.REST.Rewrite
import           Language.REST.RuntimeTerm as RT
import           Language.REST.RewriteRule

type MetaTerm = MT.MetaTerm


contains :: RuntimeTerm -> RuntimeTerm -> Bool
contains t1 t2 | t1 == t2 = True
contains (App _ ts) t     = any (contains t) ts


orient' :: Show oc => (?impl :: AbstractOC oc RuntimeTerm m) => oc -> [RuntimeTerm] -> oc
orient' oc0 ts0 = go oc0 (zip ts0 (tail ts0))
  where
    go oc []            = oc
    go oc ((t0, t1):ts) = go (refine ?impl oc t0 t1) ts

orient :: Show oc => (?impl :: AbstractOC oc RuntimeTerm m) => [RuntimeTerm] -> oc
orient = orient' (top ?impl)

canOrient :: forall oc m . Show oc
  => (?impl :: AbstractOC oc RuntimeTerm m) => [RuntimeTerm] -> m Bool
canOrient terms = trace ("Try to orient " ++ termPathStr terms) $ isSat ?impl (orient terms)

syms :: MetaTerm -> S.HashSet String
syms (MT.Var s)      = S.singleton s
syms (MT.RWApp _ xs) = S.unions (map syms xs)

termPathStr :: [RuntimeTerm] -> String
termPathStr terms = L.intercalate " --> \n" (map pp terms)
  where
    pp = prettyPrint (PPArgs [] [] (const Nothing))

eval :: S.HashSet Rewrite -> RuntimeTerm -> IO RuntimeTerm
eval rws t =
  do
    result <- mapM (apply t) (S.toList rws)
    case S.toList $ S.unions result of
      []      -> return t
      (t : _) -> eval rws t
