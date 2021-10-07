-- |

module Language.REST where

import Control.Monad.Identity
import Data.Hashable
import Data.Maybe
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import Language.REST.Types
import Language.REST.OCToAbstract
import Language.REST.OrderingConstraints
import Language.REST.OrderingConstraints.Strict (strictOC)
import Language.REST.OrderingConstraints.Lazy (lazyOC)
import Language.REST.OrderingConstraints.ADT (adtOC)
import Language.REST.RPO
import Language.REST.RuntimeTerm
import Language.REST.Op
import Language.REST.OpOrdering
import qualified Language.REST.WQO as WQO


adtRPO z3 = lift (adtOC z3) rpo
-- lazyRPO = lift lazyOC rpo
-- strictRPO = lift strictOC rpo

-- Assume vars are arity 0, which is usually correct
getVars :: RuntimeTerm -> S.HashSet Op
getVars (App op []) = S.singleton op
getVars (App op xs) = S.unions (map getVars xs)


varsEQ :: RuntimeTerm -> RuntimeTerm -> WQO.WQO Op
varsEQ t1 t2 =
  let
    vars = getVars t1 `S.union` getVars t2
  in
    fromJust $ WQO.mergeAll (map (uncurry (=.)) (pairs (S.toList vars)))
  where
    pairs xs | length xs < 2 = []
    pairs xs | otherwise = zip xs (tail xs)

cgen :: (Show (oc Op), Eq (oc Op), Hashable (oc Op)) => ConstraintGen oc Op RuntimeTerm Identity
cgen impl r oc t1 t2 =
  let
    Identity rpoc = rpo impl r oc t1 t2
  in
    return $ addConstraint impl (varsEQ t1 t2) rpoc
