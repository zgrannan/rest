{-# LANGUAGE OverloadedStrings #-}
module Lists where

import Prelude hiding (reverse)

import           Language.REST.Op
import           Language.REST.Core
import           Language.REST.Types
import qualified Language.REST.MetaTerm as MT
import           DSL

import qualified Data.HashSet as S

xs = MT.Var "xs"
ys = MT.Var "ys"

nil           = MT.RWApp "nil"     []
(.:) t1 t2    = MT.RWApp "cons"    [t1, t2]
reverse t     = MT.RWApp "reverse" [t]
(.++) lhs rhs = MT.RWApp "append"  [lhs, rhs]


evalRWs = S.fromList [
    reverse (x .: xs) ~> reverse xs .++ (x .: nil)
  , reverse nil ~> nil
  , nil .++ xs          ~> xs
  , (x .: xs) .++ ys    ~> x .: (xs .++ ys)
  , reverse (reverse (MT.RWApp "xs" [])) ~> MT.RWApp "xs" []
  ]

userRWs = S.fromList [
    reverse (xs .++ ys) ~> (reverse ys) .++ (reverse xs)
  , (reverse ys) .++ (reverse xs) ~> reverse (xs .++ ys)
  ]
