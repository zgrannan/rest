{-# LANGUAGE OverloadedStrings #-}

module MultisetOrder where

import Prelude hiding (GT)

import Control.Monad.Identity

import Language.REST.Internal.MultiSet as M
import Language.REST.Internal.MultisetOrder
import Language.REST.WQOConstraints.Strict as SC
import Language.REST.WQOConstraints as OC
import Language.REST.Types
import Language.REST.Op

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

compareOp :: ConstraintGen impl Op Op Identity
compareOp impl r oc c1 c2 = Identity $ intersectRelation impl oc (c1, c2, r)

existingOC :: StrictOC Op
existingOC = OC.intersectRelation strictOC' SC.noConstraints (Op "a", Op "c", GTE)

ms :: StrictOC Op -> MultiSet Op -> MultiSet Op -> Identity (StrictOC Op)
ms = multisetOrder compareOp strictOC' GTE

multisetNext :: Identity (StrictOC Op)
multisetNext = ms existingOC (M.fromList [Op "b", Op "a", Op "c"]) (M.fromList (replicate 4 (Op "a")))

unsat :: Identity (StrictOC Op)
unsat = do
  mn <- multisetNext
  return $ OC.intersectRelation strictOC' mn (Op "c", Op "a", GT)

tests :: TestTree
tests = testGroup "MultisetOrder"
  [ testCase "Constraints" $ assertBool "Constraints" $
      SC.noConstraints /=
        runIdentity (ms SC.noConstraints (M.fromList [Op "b", Op "c"]) (M.fromList [Op "a", Op "a"]))
  , testCase "Unsat" $ assertBool "Unsat" $
      SC.isUnsatisfiable $ runIdentity unsat
  ]
