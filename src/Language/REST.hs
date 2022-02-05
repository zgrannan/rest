-- |

module Language.REST where

import Control.Monad.Identity
import Data.Hashable
import Data.Maybe
import qualified Data.HashSet as S

import Language.REST.OCAlgebra (OCAlgebra)
import Language.REST.OCToAbstract
import Language.REST.WQOConstraints
import Language.REST.WQOConstraints.ADT (ConstraintsADT, adtOC)
import Language.REST.RPO
import Language.REST.RuntimeTerm
import Language.REST.Op
import Language.REST.Internal.OpOrdering
import qualified Language.REST.Internal.WQO as WQO
import System.IO (Handle)


adtRPO :: (Handle, Handle) -> OCAlgebra (ConstraintsADT Op) RuntimeTerm IO
adtRPO z3 = lift (adtOC z3) rpo
-- lazyRPO = lift lazyOC rpo
-- strictRPO = lift strictOC rpo

-- Assume vars are arity 0, which is usually correct
getVars :: RuntimeTerm -> S.HashSet Op
getVars (App op []) = S.singleton op
getVars (App _op xs) = S.unions (map getVars xs)


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
