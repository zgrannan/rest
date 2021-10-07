{-# LANGUAGE OverloadedStrings #-}

module Set where

import Arith as A
import Data.Text
import DSL
import Language.REST.MetaTerm
import Language.REST.Op

import qualified Data.HashSet as S

emptyset  = RWApp "∅" []

x /\ y = RWApp "intersect" [x, y]
x \/ y  = RWApp "union" [x, y]

s0 = RWApp "s₀" []
s1 = RWApp "s₁" []

isSubset t1 t2 = t1 \/ t2 ~> t2

userRWs = S.union A.evalRWs $ S.fromList $
  [
    distribL (/\) (\/)
  , distribR (/\) (\/)
  , distribL (\/) (/\)
  , distribR (\/) (/\)
  --   assocL (\/)
  -- , assocL (/\)
  , x /\ x        ~> x
  , x \/ x        ~> x
  , x \/ emptyset ~> x
  -- , commutes (\/)
  -- , commutes (/\)

  -- Example 1
  , s1 /\ s0      ~> emptyset

  -- Example 2
  -- , s0 \/ s1      ~> s0
  ]

evalRWs = S.union A.userRWs $ S.fromList --
  [ RWApp "t2" [] ~> emptyset
  , isSubset (RWApp "right1" []) (RWApp "right" [])
  ]

disjointExample  = "union(union(left, right1), union(left,right))"
disjointExample2 = "union(left, union(right1, union(left,right)))"

example1 = "f(intersect(union(s₀,s₁), s₀))"
example2 = "f(union(intersect(s₀,s₁), s₀))"
