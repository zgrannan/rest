{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}

module Language.REST.RPO (rpo, rpoTerm, rpoGTE, rpoGTE', synGTE) where

import Prelude hiding (EQ, GT)
import Debug.Trace (trace)
import Text.Printf

import Control.Monad.Identity
import Control.Monad.State.Strict
import GHC.Generics
import Data.Hashable
import qualified Data.List as L
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import qualified Language.REST.MultiSet as MS
import           Language.REST.Op
import           Language.REST.OpOrdering as OpOrdering
import           Language.REST.OrderingConstraints as OC
import qualified Language.REST.MetaTerm as MT
import           Language.REST.Types
import qualified Language.REST.RuntimeTerm as RT
import           Language.REST.MultisetOrder

type MultiSet = MS.MultiSet

data RuntimeTerm = App Op (MultiSet RuntimeTerm) deriving (Generic, Eq, Hashable, Ord)

instance Show RuntimeTerm where
  show (App op trms) =
      if MS.null trms
      then show op
      else show op ++ show trms

instance MT.ToMetaTerm RuntimeTerm where
  toMetaTerm (App op xs) = MT.RWApp op (map MT.toMetaTerm $ MS.toList xs)

ops :: RuntimeTerm -> S.HashSet Op
ops (App f ts) = S.insert f (S.unions $ map ops (MS.distinctElems ts))

rpoTerm :: RT.RuntimeTerm -> RuntimeTerm
rpoTerm (RT.App f xs) = App f $ MS.fromList (map rpoTerm xs)

isSubtermOf :: RuntimeTerm -> RuntimeTerm -> Bool
isSubtermOf t u@(App _ us) = t == u || any (t `isSubtermOf`) (MS.distinctElems us)

trace' :: String -> a -> a
-- trace' = trace
trace' _ x = x

type CacheKey oc = ((oc Op), Relation, RuntimeTerm, RuntimeTerm)

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
cached key@(_,_,t1,t2) thunk = do
  cache <- gets rpoCache
  case M.lookup key cache of
    Just result -> trace' ("Cache hit" ++ show (t1, t2)) $ return result
    Nothing     -> trace' ("Cache miss" ++ show (t1, t2)) $ do
      result <- trace' "Do thunk" thunk
      trace' "Done" $ modify (\st -> st{ rpoCache = M.insert key result (rpoCache st)})
      return result
 where
   trace' _  x = x
   -- trace' = trace


rpo :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RT.RuntimeTerm Identity
rpo = runStateConstraints (cmapConstraints rpoTerm rpo') (RPOState M.empty 0)

rpoMul :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op (MultiSet RuntimeTerm) (RMonad oc)
rpoMul = multisetOrder rpo'

rpo' :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm (RMonad oc)
rpo' impl _      oc _  _ | oc == unsatisfiable impl           = return $ unsatisfiable impl
rpo' OC{unsatisfiable} r oc      t u      | t == u            = return $ if r == GT then unsatisfiable else oc
rpo' OC{unsatisfiable} _ _       t u      | t `isSubtermOf` u = return unsatisfiable
rpo' OC{unsatisfiable} r oc      t u      | u `isSubtermOf` t = return $ if r == EQ then unsatisfiable else oc
rpo' oc r cs t@(App f ts) u@(App g us)    | f == g            = rpoMul oc r cs ts us

rpo' oc r cs t@(App f ts) u@(App g us) = incDepth result
  where
    traceString depth = printf "%s %s %s %s" (take depth $ repeat '.') (show t) (show r) (show u)
    cs'    = noConstraints oc -- relevantConstraints oc cs (ops t) (ops u)
    result = cached (cs, r, t, u) $ (intersect oc cs <$> result')
    result' = cached (cs', r, t, u) $ do
      depth <- gets rpoDepth
      trace' (traceString depth) $
        if r == EQ
        then rpoMul oc r (addConstraint oc (f =. g) cs') ts us
        else
          unionAll oc <$> sequence [
            rpoMul oc GT  (addConstraint oc (f >. g) cs') (MS.singleton t) us
          , rpoMul oc r   (addConstraint oc (f =. g) cs') ts               us
          , rpoMul oc GTE cs'                             ts               (MS.singleton u)
          ]



rpoGTE t u = runIdentity $ rpoGTE' ?impl (noConstraints ?impl) t u

rpoGTE' impl oc t u = rpo impl GTE oc t u













-- Non symbolic version

synEQ :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
synEQ o l r = synGTE' o l r && synGTE' o r l

removeSynEQs :: OpOrdering -> [RuntimeTerm] -> [RuntimeTerm] -> ([RuntimeTerm], [RuntimeTerm])
removeSynEQs _ [] ys = ([], ys)
removeSynEQs ordering (x : xs) ys
  | Just y <- L.find (synEQ ordering x) ys
  = removeSynEQs ordering xs $ L.delete y ys
  | otherwise
  = let (xs', ys') = removeSynEQs ordering xs ys in (x : xs', ys')

synGTE :: OpOrdering -> RT.RuntimeTerm -> RT.RuntimeTerm -> Bool
synGTE o t u = synGTE' o (rpoTerm t) (rpoTerm u)

synGTE' :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
synGTE' ordering t@(App f ts) u@(App g us)
  | opGT ordering f g
  = synGTM ordering (MS.singleton t) us
synGTE' ordering (App f ts) (App g us)
  | opEQ ordering f g
  = synGTEM ordering ts us
synGTE' ordering (App _ ts) u = synGTEM ordering ts (MS.singleton u)

rpoT :: OpOrdering -> RuntimeTerm -> RuntimeTerm -> Bool
rpoT o t1 t2 = synGTE' o t1 t2 && not (synGTE' o t2 t1)

synGTEM :: OpOrdering -> MultiSet RuntimeTerm -> MultiSet RuntimeTerm -> Bool
synGTEM ordering xs ys = case removeSynEQs ordering (MS.toList xs) (MS.toList ys) of
  (xs', ys') -> all (\y -> any (\x -> rpoT ordering x y) xs') ys'

synGTM :: OpOrdering -> MultiSet RuntimeTerm -> MultiSet RuntimeTerm -> Bool
synGTM ordering xs ys = case removeSynEQs ordering (MS.toList xs) (MS.toList ys) of
  ([] , [] ) -> False
  (xs', ys') -> all (\y -> any (\x -> rpoT ordering x y) xs') ys'
