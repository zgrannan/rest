{-# LANGUAGE OverloadedStrings #-}

module Multiset where

import Data.Text ()
import DSL
import Language.REST.MetaTerm
import Language.REST.Internal.Rewrite
import qualified Data.HashSet as S

(\/) :: MetaTerm -> MetaTerm -> MetaTerm
x \/ y  = RWApp "union" [x, y]

singleton, multisetOf :: MetaTerm -> MetaTerm
singleton x = RWApp "m" [x]
multisetOf x = RWApp "toMS" [x]

cons :: MetaTerm -> MetaTerm -> MetaTerm
cons x y = RWApp "cons" [x, y]

ite :: MetaTerm -> MetaTerm -> MetaTerm -> MetaTerm
ite a b c = RWApp "ite" [a,b,c]

hd, tl, isEmpty :: MetaTerm -> MetaTerm
hd x = RWApp "head" [x]
tl x = RWApp "tail" [x]
isEmpty x = RWApp "isEmpty" [x]

empty :: MetaTerm
empty = RWApp "empty" []

xs, ys :: MetaTerm
xs = RWApp "xs" []
ys = RWApp "ys" []

expandM :: MetaTerm -> Rewrite
expandM xs = multisetOf xs ~> ite (isEmpty xs) empty (singleton (hd xs) \/ multisetOf (tl xs))

userRWs :: S.HashSet Rewrite
userRWs = S.fromList $
  [
    commutes (\/) `named` "mpComm"
  , assocL (\/) `named` "mpAssoc"
  , assocR (\/) `named` "mpAssoc"
  , (singleton x) \/ (multisetOf y) ~> multisetOf (cons x y)
  ]

evalRWs :: S.HashSet Rewrite
evalRWs = S.fromList
  [ multisetOf (cons x y) ~> (singleton x) \/ (multisetOf y)
  , expandM xs
  , expandM ys
  ]

perm1t, perm1s :: String
perm1s = "union(m(y), union(toMS(cons(a,xs)),toMS(ys)))"
perm1t = "union(m(a), union(toMS(xs), toMS(cons(y,ys))))"
