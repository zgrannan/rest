{-# LANGUAGE OverloadedStrings #-}
module Arith where

import Data.Text
import DSL
import Language.REST.MetaTerm
import Language.REST.Op

import qualified Data.HashSet as S

neg :: MetaTerm -> MetaTerm
neg x = RWApp (Op "neg") [x]

double x = RWApp (Op "double") [x]
twicePlus x y = RWApp (Op "twicePlus") [x, y]

(<#) :: MetaTerm -> MetaTerm -> MetaTerm
x <# y = RWApp (Op "<") [x, y]

evalRWs =
    S.fromList
      [
        (suc' x) <# (suc' y) ~> x <# y
      , (suc' x) #+ y ~> suc' (x #+ y)
      , zero'    #+ x ~> x

      , (suc' x) #* y ~> y #+ (x #* y)
      , zero'     #* y ~> zero'

      , ack' zero' x           ~> suc' x
      , ack' (suc' x) zero'    ~> ack' x one'
      , ack' (suc' x) (suc' y) ~> ack' x (ack' (suc' x) y)
      , double x               ~> x #+ x
      , twicePlus x y          ~> (x #+ x) #+ y
      ]

userRWs =
    S.fromList $
      [ x #+ y        ~> y #+ x

      , x #* y        ~> y #* x

      , (x #+ y) #* v ~> (x #* v) #+ (y #* v)
      , (neg x) #+ x ~> zero'
      -- , (x #* v) #+ (y #* v) ~> (x #+ y) #* v

      --  , x ~> x #+ zero'
      ] ++ [ x #+ (y #+ v) ~> (x #+ y) #+ v]
