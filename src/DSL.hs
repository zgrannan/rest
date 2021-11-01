{-# LANGUAGE OverloadedStrings #-}

module DSL where

import           Language.REST.Op
import qualified Language.REST.MetaTerm as MT
import           Language.REST.RuntimeTerm as RT
import           Language.REST.Rewrite
import           Nat

commutes op      = x `op` y            ~> y `op` x
assocL   op      = (x `op` y) `op` z'   ~> x `op` (y `op` z')
assocR   op      = x `op` (y `op` z')   ~> (x `op` y) `op` z'
distribL op1 op2 = (x `op1` y) `op2` z' ~> (x `op2` z') `op1` (y `op2` z')
distribR op1 op2 = z' `op2` (x `op1` y) ~> (z' `op2` x) `op1` (z' `op2` y)

ackOp  = Op "ack"
plus   = Op "+"
minus  = Op "-"
times  = Op "*"

a = App (Op "a") []
b = App (Op "b") []
c = App (Op "c") []
d = App (Op "d") []
x = MT.Var "x"
y = MT.Var "y"
v = MT.Var "v"
w = MT.Var "w"
z' = MT.Var "z"

f = Op "f"
g = Op "g"
h = Op "h"

t1Op = Op "t1"
t2Op = Op "t2"

t1 = App (Op "t1") []
t2 = App (Op "t2") []
t3 = App (Op "t3") []
t4 = App (Op "t4") []
t5 = App (Op "t5") []

zero    = App z []
one     = suc zero
two     = suc one
suc x   = App s [x]
ack x y = App ackOp [x, y]

zero'    = MT.toMetaTerm zero
one'     = suc' zero'
suc' x   = MT.RWApp s [x]
ack' x y = MT.RWApp ackOp [x, y]

infixl 1 .+
(.+) :: RuntimeTerm -> RuntimeTerm -> RuntimeTerm
(.+) x y = App plus [x, y]

(#+) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#+) x y = MT.RWApp plus [x, y]

(#-) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#-) x y = MT.RWApp minus [x, y]

(#*) :: MT.MetaTerm -> MT.MetaTerm -> MT.MetaTerm
(#*) x y = MT.RWApp times [x, y]

infix 0 ~>
(~>) :: MT.MetaTerm -> MT.MetaTerm -> Rewrite
t ~> u = Rewrite t u Nothing

infix 0 <~>
(<~>) :: MT.MetaTerm -> MT.MetaTerm -> [Rewrite]
t <~> u = [ t ~> u, u ~> t ]
