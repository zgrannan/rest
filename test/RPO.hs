{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module RPO where

import Debug.Trace
import Control.Monad.Identity
import Data.Hashable
import DSL
import Nat
import Language.REST.Op
import Language.REST.OpOrdering as OpOrdering
import Language.REST.OrderingConstraints as OC
import Language.REST.RuntimeTerm
import Language.REST.RPO
import Language.REST.WQO

import Data.Maybe as Mb

bigLeft = "f(h(s(g(z,nil))),f(g(z,z),nil,h(z)),f(z,nil,z) + g(z,s(z)))"
bigRight = "g(g(g(s(nil),z),s(z) + z),g(s(s(h(nil))),s(z)))"

massiveLeft = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

massiveRight = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isnil(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By, cons(lqdcselectcons1(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), concat(lqdcselectcons2(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By))))"

listsLeft = "concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), concat(reverse(lqdcselectcons2(dsdOz)), cons(lqdcselectcons1(dsdOz), Nil)))"

listsRight = "concat(concat(ite(isNil(ysaLe), Nil, concat(reverse(lqdcselectcons2(ysaLe)), cons(lqdcselectcons1(ysaLe), Nil))), reverse(lqdcselectcons2(dsdOz))), cons(lqdcselectcons1(dsdOz), Nil))"

flattenLeft2 = "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))"

flattenRight2 = "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"

flattenSeq = [
        "concat(flatten(la1Bz), concat(flatten(ra1BA), nsa1By))",
        "concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), concat(ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA)))), nsa1By))",
        "concat(concat(ite(isLeaf(la1Bz), cons(LeaflqdcselectLeaf1(la1Bz), nil), concat(flatten(NodelqdcselectNode1(la1Bz)), flatten(NodelqdcselectNode2(la1Bz)))), ite(isLeaf(ra1BA), cons(LeaflqdcselectLeaf1(ra1BA), nil), concat(flatten(NodelqdcselectNode1(ra1BA)), flatten(NodelqdcselectNode2(ra1BA))))), nsa1By)"
  ]

rpoSeq xs = go (OC.noConstraints ?impl) xs where
  go c (t:u:xs) = OC.intersect ?impl c (rpoGTE t u)
  go c _        = c


tests :: (Hashable (oc Op), Eq (oc Op), Show (oc Op)) => (?impl :: OrderingConstraints oc IO) => [(String, IO Bool)]
tests =
  let
    f = Op "f"
    g = Op "g"
    h = Op "h"
  in
    [ ("RPO1",   return $ (rpoGTE "f(z)" "g(s(z))")
              == OC.intersect ?impl (OC.singleton ?impl (f >. g)) (OC.singleton ?impl (f >. s)))
    , ("RPO2", isUnsatisfiable ?impl $
         OC.intersect ?impl
           (rpoGTE "f(z)" "g(s(z))")
           (rpoGTE "g(s(z))" "f(h(z))"))
    , ("RPO3", isSatisfiable ?impl (rpoGTE bigLeft bigRight))
    , ("RPO4", isUnsatisfiable ?impl (rpoGTE massiveLeft massiveRight))
    , ("RPO5", isSatisfiable ?impl (rpoGTE listsLeft listsRight))
    , ("RPO6", isSatisfiable ?impl (rpoGTE flattenLeft2 flattenRight2))
    , ("RPOOrient", isUnsatisfiable ?impl $ rpoSeq flattenSeq)
    , ("SynGTE", return $ synGTE OpOrdering.empty (App s [App s [App g [App (Op "+") [App h [App s [App z []]],App z []],App s [App s [App g [App z [],App z []]]]]]]) (App z []))
    , ("SynGTE2",
        return $ synGTE (Mb.fromJust $ mergeAll [
          ("cons" >. g)
        , (f >. s)
        , (h >. g)
        , (h >. "nil")]) "s(cons(h(h(z)), f(nil, nil, z)))"  "g(z, cons(g(nil, nil), s(s(z))))")
    ]
