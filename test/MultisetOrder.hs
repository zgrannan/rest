module MultisetOrder where

import Prelude hiding (GT)


import Control.Monad.Identity
import Debug.Trace (trace)

import Language.REST.MultiSet as M
import Language.REST.MultisetOrder
import Language.REST.OrderingConstraints.Strict as SC
import Language.REST.OrderingConstraints as OC
import Language.REST.Types

compareChar :: ConstraintGen impl Char Char Identity
compareChar impl r oc c1 c2 = Identity $ intersectRelation impl oc (c1, c2, r)

existingOC = OC.intersectRelation strictOC' SC.noConstraints ('a', 'c', GTE)

ms = multisetOrder compareChar strictOC' GTE

multisetNext = ms existingOC (M.fromList "bac") (M.fromList "aaaa")

unsat :: Identity (StrictOC Char)
unsat = do
  mn <- multisetNext
  return $ OC.intersectRelation strictOC' mn ('c', 'a', GT)

tests :: [(String, Bool)]
tests = [
    ("Constraints",
     (SC.noConstraints /=
     (runIdentity $ ms SC.noConstraints (M.fromList "bc") (M.fromList "aa"))))
  , ("Unsat", SC.isUnsatisfiable $ runIdentity unsat)
  ]
