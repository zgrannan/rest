{-# LANGUAGE OverloadedStrings #-}

module NonTerm where

import Arith as A
import Data.Text
import DSL
import Nat
import Language.REST.Op
import Language.REST.MetaTerm
import Language.REST.Rewrite
import qualified Data.HashSet as S

a' x = RWApp (Op "a") [x]
b' x = RWApp (Op "b") [x]
c' x = RWApp (Op "c") [x]
d' x = RWApp (Op "d") [x]

userRWs :: S.HashSet Rewrite
userRWs = S.fromList $
  [
    a' (b' x) ~> a' (d' x)
  , d' (b' x) ~> b' (d' x)
  , b' (d' x) ~> d' (b' x)
  , d' (b' x) ~> b' (b' (b' x))
  ]


evalRWs = A.evalRWs
