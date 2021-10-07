{-# LANGUAGE OverloadedStrings #-}

module Multiset where

import Data.Text ()
import DSL
import Language.REST.MetaTerm
import Language.REST.Op
import Language.REST.Rewrite
import qualified Data.HashSet as S
import Prelude hiding (singleton)

x \/ y  = RWApp "union" [x, y]

singleton x = RWApp "m" [x]
cons x y = RWApp "cons" [x, y]
multisetOf x = RWApp "toMS" [x]
ite a b c = RWApp "ite" [a,b,c]
hd x = RWApp "head" [x]
tl x = RWApp "tail" [x]
isEmpty x = RWApp "isEmpty" [x]
empty = RWApp "empty" []

xs = RWApp "xs" []
ys = RWApp "ys" []

expandM xs = multisetOf xs ~> ite (isEmpty xs) empty (singleton (hd xs) \/ multisetOf (tl xs))

userRWs = S.fromList $
  [
    commutes (\/) `named` "mpComm"
  , assocL (\/) `named` "mpAssoc"
  , assocR (\/) `named` "mpAssoc"
  , (singleton x) \/ (multisetOf y) ~> multisetOf (cons x y)
  ]

evalRWs = S.fromList
  [ multisetOf (cons x y) ~> (singleton x) \/ (multisetOf y)
  , expandM xs
  , expandM ys
  ]

perm1s = "union(m(y), union(toMS(cons(a,xs)),toMS(ys)))"
perm1t = "union(m(a), union(toMS(xs), toMS(cons(y,ys))))"
