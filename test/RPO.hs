{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RPO where

import Data.Hashable
import Nat
import Language.REST.Op
import Language.REST.Internal.OpOrdering as OpOrdering
import Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.Strict as SC (strictOC)
import Language.REST.RuntimeTerm
import Language.REST.RPO
import Language.REST.Internal.WQO

import Data.Maybe as Mb

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

bigLeft, bigRight :: RuntimeTerm
bigLeft = "f(h(s(g(z,nil))),f(g(z,z),nil,h(z)),f(z,nil,z) + g(z,s(z)))"
bigRight = "g(g(g(s(nil),z),s(z) + z),g(s(s(h(nil))),s(z)))"

massiveLeft :: RuntimeTerm
massiveLeft = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

massiveRight :: RuntimeTerm
massiveRight = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isnil(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By, cons(lqdcselectcons1(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), concat(lqdcselectcons2(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By))))"

listsLeft :: RuntimeTerm
listsLeft = "concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), concat(reverse(lqdcselectcons2(dsdOz)), cons(lqdcselectcons1(dsdOz), Nil)))"

listsRight :: RuntimeTerm
listsRight = "concat(concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), reverse(lqdcselectcons2(dsdOz))), cons(lqdcselectcons1(dsdOz), Nil))"

flattenLeft2 :: RuntimeTerm
flattenLeft2 = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

flattenRight2 :: RuntimeTerm
flattenRight2 = "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"

flattenSeq :: [RuntimeTerm]
flattenSeq = [
        "concat(flatten(la1Bz), concat(flatten(ra1BA), nsa1By))",
        "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))",
        "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"
  ]

rpoSeq
  :: (?impl::WQOConstraints impl m, Show (impl Op), Eq (impl Op), Hashable (impl Op))
  => [RuntimeTerm]
  -> impl Op
rpoSeq = go (OC.noConstraints ?impl) where
  go c (t:u:_xss) = OC.intersect ?impl c (rpoGTE t u)
  go c _        = c


tests :: TestTree
tests =
  let ?impl = SC.strictOC @IO in
  let
    f = Op "f"
    g = Op "g"
    h = Op "h"
  in
    testGroup "RPO"
      [ testCase "RPO1" $ assertBool "RPO1" $
          rpoGTE "f(z)" "g(s(z))"
            == OC.intersect ?impl (OC.singleton ?impl (f >. g)) (OC.singleton ?impl (f >. s))
      , testCase "SynGTE" $ assertBool "SynGTE" $
          synGTE OpOrdering.empty (App s [App s [App g [App (Op "+") [App h [App s [App z []]],App z []],App s [App s [App g [App z [],App z []]]]]]]) (App z [])
      , testCase "SynGTE2" $ assertBool "SynGTE2" $
          synGTE (Mb.fromJust $ mergeAll [
            "cons" >. g
          , f >. s
          , h >. g
          , h >. "nil"]) "s(cons(h(h(z)), f(nil, nil, z)))"  "g(z, cons(g(nil, nil), s(s(z))))"
      ]

-- | Satisfiability tests. These must run against a solver-backed instance
--   ('smtOC'), because folding a long RPO sequence through a DNF instance
--   ('Strict') expands exponentially.
solverTests :: (Hashable (oc Op), Eq (oc Op), Show (oc Op)) => (?impl :: WQOConstraints oc IO) => TestTree
solverTests =
  testGroup "RPO-solver"
    [ testCase "RPO2" $ do
        isUnsat <- isUnsatisfiable ?impl
          ( OC.intersect ?impl
              (rpoGTE "f(z)" "g(s(z))")
              (rpoGTE "g(s(z))" "f(h(z))")
          )
        assertBool "RPO2" isUnsat
    , testCase "RPO3" $ isSatisfiable ?impl (rpoGTE bigLeft bigRight) >>= assertBool "RPO3"
    , testCase "RPO4" $ isUnsatisfiable ?impl (rpoGTE massiveLeft massiveRight) >>= assertBool "RPO4"
    , testCase "RPO5" $ isSatisfiable ?impl (rpoGTE listsLeft listsRight) >>= assertBool "RPO5"
    , testCase "RPO6" $ isSatisfiable ?impl (rpoGTE flattenLeft2 flattenRight2) >>= assertBool "RPO6"
    , testCase "RPOOrient" $ isUnsatisfiable ?impl (rpoSeq flattenSeq) >>= assertBool "RPOOrient"
    ]
