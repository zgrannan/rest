{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import Data.Text
import qualified Arith as A
import DSL
import Language.REST.MetaTerm
import Language.REST.Op

import qualified Data.HashSet as S
import Prelude hiding (repeat, seq)

repeat n op = RWApp (Op "repeat") [n, op]
seq op1 op2 = RWApp (Op "seq") [op1, op2]
nop         = RWApp (Op "nop") []


userRWs =
  S.union A.userRWs
     (S.fromList $ [
         seq x nop      ~> x
       , seq nop x      ~> x
       , repeat zero' x ~> nop
     ] ++ (repeat (suc' y) x <~> seq x (repeat y x))
       -- ++ (repeat (suc' y) x <~> seq (repeat y x) x)
       ++ (repeat (suc' (suc' zero')) x <~> seq x x))

evalRWs = S.empty -- S.fromList [  ]
